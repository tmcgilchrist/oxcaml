(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* typetexp.ml,v 1.34.4.9 2002/01/07 08:39:16 garrigue Exp *)

(* Typechecking of type expressions for the core language *)

open Asttypes
open Misc
open Parsetree
open Typedtree
open Types
open Mode
open Ctype

exception Already_bound

type unbound_variable_policy =
  | Open (* common case *)
  | Closed (* no wildcards or unqunatified variables allowed *)
  | Closed_for_upstream_compatibility (* same as above, extra error hint *)

type unbound_variable_reason = | Upstream_compatibility

(* A way to specify what jkind should be used for new type variables:

   [Sort] means to initialize variables with representable jkinds (sort
   varibales internally) and the jkinds will get defaulted to value if
   it remains unconstrained.

   [Any] means to initialize the variables with jkind any. No defaulting
   will occur in this case. *)
type jkind_initialization_choice = Sort | Any

type value_loc =
    Tuple | Poly_variant | Object_field

type sort_loc =
    Fun_arg | Fun_ret

type cannot_quantify_reason =
  | Unified of type_expr
  | Univar
  | Scope_escape

(* a description of the jkind on an explicitly quantified universal
   variable, containing whether the jkind was a default
   (e.g. [let f : 'a. 'a -> 'a = ...]) or explicit
   (e.g. [let f : ('a : immediate). ...]) and what the jkind was;
   it is original as compared to the inferred jkind after processing
   the body of the type *)
type jkind_info =
  { original_jkind : jkind_lr;
    defaulted : bool;
  }

type error =
  | Unbound_type_variable of
    string * string list * unbound_variable_reason option
  | No_type_wildcards of unbound_variable_reason option
  | Undefined_type_constructor of Path.t
  | Type_arity_mismatch of Longident.t * int * int
  | Bound_type_variable of string
  | Recursive_type
  | Type_mismatch of Errortrace.unification_error
  | Alias_type_mismatch of Errortrace.unification_error
  | Present_has_conjunction of string
  | Present_has_no_type of string
  | Constructor_mismatch of type_expr * type_expr
  | Not_a_variant of type_expr
  | Variant_tags of string * string
  | Invalid_variable_name of string
  | Cannot_quantify of string * cannot_quantify_reason
  | Bad_univar_jkind of
      { name : string; jkind_info : jkind_info; inferred_jkind : jkind_lr }
  | Multiple_constraints_on_type of Longident.t
  | Method_mismatch of string * type_expr * type_expr
  | Opened_object of Path.t option
  | Not_an_object of type_expr
  | Unsupported_extension : _ Language_extension.t -> error
  | Polymorphic_optional_param
  | Non_value of
      {vloc : value_loc; typ : type_expr; err : Jkind.Violation.t}
  | Non_sort of
      {vloc : sort_loc; typ : type_expr; err : Jkind.Violation.t}
  | Bad_jkind_annot of type_expr * Jkind.Violation.t
  | Did_you_mean_unboxed of Longident.t
  | Invalid_label_for_call_pos of Parsetree.arg_label

exception Error of Location.t * Env.t * error
exception Error_forward of Location.error

(* Note [Global type variables]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   The "global" type variables are ones that exist outside of just one
   particular type. These include the type parameters of a type declaration
   and the type parameters introduced in a type extension constructor,
   for example.

   When we are translating a type variable (in function [transl_type_var]), we
   first look for a locally-in-scope variable -- this is one already used in
   that same type. If that lookup fails, we will then invent a new variable.
   When we're done translating the type, we'll call [globalize_used_variables],
   which unifies the local variables with any global ones of the same name.
   (Alternative possible plan: just use the global variables directly when
   no local variable is in scope. Maybe this would be better?)

   This plan works, but there is a downside: it means that jkind information
   on the global doesn't propagate to the local. Here is an example:

   {[
     type ('a : float64) t = 'a * 'a
   ]}

   As of the time of writing, the tuple type will require ['a] to have jkind
   [value], and the unification in [globalize_used_variables] will fail. But
   it fails on the jkind signature for the type variable, which seems confusing.
   Surely the jkind signature should tell us what the jkind of the variable
   is and we should just use this in the RHS.

   So we propagate the jkind information in [transl_type_var] by looking up
   the global type variable and getting its jkind. This is not so easy, though:
   the global might have been unified (in the case of [constraint]s, say) and
   so its jkind might be an l-jkind, not an r-jkind. Yet we need an r-jkind to
   give as the jkind of the fresh local type variable.

   The solution to this little problem is that we remember the original r-jkind
   of global type variables separately. Then we use this original r-jkind in
   [transl_type_var]. This is a tiny bit wrong in some sense: if the global has
   been unified with something, its jkind might have been improved from the
   original annotation, and this improvement is not propagated to
   [transl_type_var]. But this is OK, for two reasons: 1) we really only need
   to propagate the original annotation for good error messages, and 2) the
   type variables get unified in [globalize_used_variables] anyway.
*)

module TyVarEnv : sig
  val reset : unit -> unit
  (* see mli file *)

  val is_in_scope : string -> bool

  val add : string -> type_expr -> jkind_lr -> unit
  (* add a global type variable to the environment, with the given jkind.
     Precondition: the [type_expr] must be a [Tvar] with the given jkind. *)

  val with_local_scope : (unit -> 'a) -> 'a
  (* see mli file *)

  type poly_univars
  val with_univars : poly_univars -> (unit -> 'a) -> 'a
  (* evaluate with a locally extended set of univars *)

  val ttyp_poly_arg :
    poly_univars -> (string * Parsetree.jkind_annotation option) list
  (* something suitable as an argument to [Ttyp_poly] *)

  val make_poly_univars : string Location.loc list -> poly_univars
  (* a version of [make_poly_univars_jkinds] that doesn't take jkinds *)

  val make_poly_univars_jkinds :
    context:(string -> Jkind.History.annotation_context_lr) ->
    (string Location.loc * Parsetree.jkind_annotation option) list
    -> poly_univars
  (* see mli file *)

  val check_poly_univars : Env.t -> Location.t -> poly_univars -> type_expr list
  (* see mli file *)

  val instance_poly_univars :
     Env.t -> Location.t -> poly_univars -> type_expr list
  (* see mli file *)

  type policy
  val make_policy :
    unbound_variable_policy -> jkind_initialization_choice -> policy
  val univars_policy : policy
    (* fresh variables are univars (in methods), with representable jkinds *)
  val new_any_var : Location.t -> Env.t -> jkind_lr -> policy -> type_expr
    (* create a new variable to represent a _; fails for fixed policy *)
  val new_var : ?name:string -> jkind_lr -> policy -> type_expr
    (* create a new variable according to the given policy *)

  val new_jkind : is_named:bool -> policy -> jkind_lr
    (* create a new jkind depending on the current policy *)

  val add_pre_univar : type_expr -> policy -> unit
    (* remember that a variable might become a univar if it isn't unified;
       used for checking method types *)

  val collect_univars : (unit -> 'a) -> 'a * type_expr list
    (* collect univars during a computation; returns the univars.
       The wrapped computation should use [univars_policy].
       postcondition: the returned type_exprs are all Tunivar *)

  val reset_locals : ?univars:poly_univars -> unit -> unit
    (* clear out the local type variable env't; call this when starting
       a new e.g. type signature. Optionally pass some univars that
       are in scope. *)

  val lookup_local :
    row_context:type_expr option ref list -> string -> type_expr
    (* look up a local type variable; throws Not_found if it isn't in scope *)

  val lookup_global_jkind : string -> jkind_lr
    (* look up a global type variable, returning the jkind it was originally
       assigned. Throws [Not_found] if the variable isn't in scope. See
       Note [Global type variables]. *)

  val remember_used : string -> type_expr -> Location.t -> unit
    (* remember that a given name is bound to a given type *)

  val globalize_used_variables : policy -> Env.t -> unit -> unit
   (* after finishing with a type signature, used variables are unified to the
      corresponding global type variables if they exist. Otherwise, in function
      of the policy, fresh used variables are either
        - added to the global type variable scope if they are not longer
        variables under the fixed policy
        - added to the global type variable scope under the extensible policy
        - expected to be collected later by a call to `collect_univar` under the
        {!universal_policy}
      Reading Note [Global type variables] may also be helpful.
   *)

end = struct
  (** Map indexed by type variable names. *)
  module TyVarMap = Misc.Stdlib.String.Map

  let not_generic v = get_level v <> Btype.generic_level

  (* These are the "global" type variables: they were in scope before
     we started processing the current type. See Note [Global type variables].
  *)
  let type_variables = ref (TyVarMap.empty : (type_expr * jkind_lr) TyVarMap.t)

  (* These are variables that have been used in the currently-being-checked
     type, possibly including the variables in [type_variables].
  *)
  let used_variables =
    ref (TyVarMap.empty : (type_expr * Location.t) TyVarMap.t)

  (* These are variables that will become univars when we're done with the
     current type. Used to force free variables in method types to become
     univars.
  *)
  let pre_univars = ref ([] : type_expr list)

  let reset () =
    reset_global_level ();
    type_variables := TyVarMap.empty

  let is_in_scope name =
    TyVarMap.mem name !type_variables

  let add name v jkind =
    assert (not_generic v);
    type_variables := TyVarMap.add name (v, jkind) !type_variables

  let narrow () =
    (increase_global_level (), !type_variables)

  let widen (gl, tv) =
    restore_global_level gl;
    type_variables := tv

  let with_local_scope f =
   let context = narrow () in
   Fun.protect
     f
     ~finally:(fun () -> widen context)

  (* throws Not_found if the variable is not in scope *)
  let lookup_global name =
    fst (TyVarMap.find name !type_variables)

  let lookup_global_jkind name =
    snd (TyVarMap.find name !type_variables)

  let get_in_scope_names () =
    let add_name name _ l =
      if name = "_" then l else Pprintast.tyvar_of_name name :: l
    in
    TyVarMap.fold add_name !type_variables []

  (*****)
  (* These are variables we expect to become univars (they were introduced with
     e.g. ['a .]), but we need to make sure they don't unify first.  Why not
     just birth them as univars? Because they might successfully unify with a
     row variable in the ['a. < m : ty; .. > as 'a] idiom.  They are like the
     [used_variables], but will not be globalized in [globalize_used_variables].
  *)
  type pending_univar = {
    univar: type_expr  (** the univar itself *);
    mutable associated: type_expr option ref list;
     (** associated references to row variables that we want to generalize
       if possible *)
    jkind_info : jkind_info (** the original kind *)
  }

  type poly_univars = (string * pending_univar) list

  let univars = ref ([] : poly_univars)
  let assert_univars uvs =
    assert (List.for_all (fun (_name, v) -> not_generic v.univar) uvs)

  let rec find_poly_univars name = function
    | [] -> raise Not_found
    | (n, t) :: rest ->
      if String.equal name n
      then t
      else find_poly_univars name rest

  let with_univars new_ones f =
    assert_univars new_ones;
    let old_univars = !univars in
    univars := new_ones @ !univars;
    Fun.protect
      f
      ~finally:(fun () -> univars := old_univars)

  let ttyp_poly_arg (poly_univars : poly_univars) = List.map
      (fun (name, pending_univar) ->
        name,
        Jkind.get_annotation pending_univar.jkind_info.original_jkind)
      poly_univars

  let mk_pending_univar name jkind jkind_info =
    { univar = newvar ~name jkind; associated = []; jkind_info }

  let mk_poly_univars_tuple_with_jkind ~context var jkind_annot =
    let name = var.txt in
    let original_jkind =
      Jkind.of_annotation ~context:(context name) jkind_annot
    in
    let jkind_info = { original_jkind; defaulted = false } in
    name, mk_pending_univar name original_jkind jkind_info

  let mk_poly_univars_tuple_without_jkind var =
    let name = var.txt in
    let original_jkind = Jkind.Builtin.value ~why:Univar in
    let jkind_info =
      { original_jkind; defaulted = true }
    in
    name, mk_pending_univar name original_jkind jkind_info

  let make_poly_univars vars =
    List.map mk_poly_univars_tuple_without_jkind vars

  let make_poly_univars_jkinds ~context vars_jkinds =
    let mk_trip = function
        | (v, None) -> mk_poly_univars_tuple_without_jkind v
        | (v, Some l) -> mk_poly_univars_tuple_with_jkind ~context v l
    in
    List.map mk_trip vars_jkinds

  let promote_generics_to_univars promoted vars =
      List.fold_left
        (fun acc v ->
           match get_desc v with
           | Tvar { name; jkind } when get_level v = Btype.generic_level ->
               set_type_desc v (Tunivar { name; jkind });
               v :: acc
           | _ -> acc
        )
        promoted vars

  let check_poly_univars env loc vars =
    vars |> List.iter (fun (_, p) -> generalize p.univar);
    let univars =
      vars |> List.map (fun (name, {univar=ty1; jkind_info; _ }) ->
      let v = Btype.proxy ty1 in
      let cant_quantify reason =
        raise (Error (loc, env, Cannot_quantify(name, reason)))
      in
      begin match get_desc v with
      | Tvar { jkind } when
          not (Jkind.equate jkind jkind_info.original_jkind) ->
        let reason =
          Bad_univar_jkind { name; jkind_info; inferred_jkind = jkind }
        in
        raise (Error (loc, env, reason))
      | Tvar _ when get_level v <> Btype.generic_level ->
          cant_quantify Scope_escape
      | Tvar { name; jkind } ->
         set_type_desc v (Tunivar { name; jkind })
      | Tunivar _ ->
         cant_quantify Univar
      | _ ->
         cant_quantify (Unified v)
      end;
      v)
    in
    (* Since we are promoting variables to univars in
       {!promote_generics_to_univars}, even if a row variable is associated with
       multiple univars we will promote it once, when checking the nearest
       univar associated to this row variable.
    *)
    let promote_associated acc (_,v) =
      let enclosed_rows = List.filter_map (!) v.associated in
      promote_generics_to_univars acc enclosed_rows
    in
    List.fold_left promote_associated univars vars

  let instance_poly_univars env loc vars =
    let vs = check_poly_univars env loc vars in
    vs |> List.iter (fun v ->
      match get_desc v with
      | Tunivar { name; jkind } ->
         set_type_desc v (Tvar { name; jkind })
      | _ -> assert false);
    vs

  (*****)
  let reset_locals ?univars:(uvs=[]) () =
    assert_univars uvs;
    univars := uvs;
    used_variables := TyVarMap.empty

  let associate row_context p =
    let add l x = if List.memq x l then l else x :: l in
    p.associated <- List.fold_left add row_context p.associated

  (* throws Not_found if the variable is not in scope *)
  let lookup_local ~row_context name =
    try
      let p = find_poly_univars name !univars in
      associate row_context p;
      p.univar
    with Not_found ->
      instance (fst (TyVarMap.find name !used_variables))
      (* This call to instance might be redundant; all variables
         inserted into [used_variables] are non-generic, but some
         might get generalized. *)

  let remember_used name v loc =
    assert (not_generic v);
    used_variables := TyVarMap.add name (v, loc) !used_variables


  type flavor = Unification | Universal
  type policy = {
    flavor : flavor;
    unbound_variable_policy : unbound_variable_policy;
    jkind_initialization: jkind_initialization_choice;
  }

  let make_policy unbound_variable_policy jkind_initialization = {
    flavor = Unification;
    unbound_variable_policy;
    jkind_initialization;
  }

  let univars_policy = {
    flavor = Universal;
    unbound_variable_policy = Open;
    jkind_initialization = Sort;
  }

  let add_pre_univar tv = function
    | { flavor = Universal } ->
      assert (not_generic tv);
      pre_univars := tv :: !pre_univars
    | _ -> ()

  let collect_univars f =
    pre_univars := [];
    let result = f () in
    let univs = promote_generics_to_univars [] !pre_univars in
    result, univs

  let new_var ?name jkind policy =
    let tv = Ctype.newvar ?name jkind in
    add_pre_univar tv policy;
    tv

  let new_jkind ~is_named { jkind_initialization } =
    match jkind_initialization with
    (* CR layouts v3.0: while [Any] case allows nullable jkinds, [Sort] does not.
       From testing, we need all callsites that use [Sort] to be non-null to
       preserve backwards compatibility. But we also need [Any] callsites
       to accept nullable jkinds to allow cases like [type ('a : value_or_null) t = 'a]. *)
    | Any -> Jkind.Builtin.any ~why:(if is_named then Unification_var else Wildcard)
    | Sort -> Jkind.of_new_legacy_sort ~why:(if is_named then Unification_var else Wildcard)

  let new_any_var loc env jkind = function
    | { unbound_variable_policy = Closed; _ } ->
      raise(Error(loc, env, No_type_wildcards None))
    | { unbound_variable_policy = Closed_for_upstream_compatibility; _ } ->
      raise(Error(loc, env, No_type_wildcards (Some Upstream_compatibility)))
    | policy -> new_var jkind policy

  let globalize_used_variables
      { flavor; unbound_variable_policy; _ } env =
    let r = ref [] in
    TyVarMap.iter
      (fun name (ty, loc) ->
        if flavor = Unification || is_in_scope name then
          let v = new_global_var (Jkind.Builtin.any ~why:Dummy_jkind) in
          let snap = Btype.snapshot () in
          if try unify env v ty; true with _ -> Btype.backtrack snap; false
          then try
            r := (loc, v, lookup_global name) :: !r
          with Not_found ->
            match unbound_variable_policy, Btype.is_Tvar ty with
            | Open, _ | (Closed | Closed_for_upstream_compatibility), false ->
              let jkind = Jkind.Builtin.any ~why:Dummy_jkind in
              let v2 = new_global_var jkind in
              r := (loc, v, v2) :: !r;
              add name v2 jkind
            | Closed, true ->
              raise(Error(loc, env,
                          Unbound_type_variable (Pprintast.tyvar_of_name name,
                                                 get_in_scope_names (),
                                                 None)))
            | Closed_for_upstream_compatibility, true ->
              raise(Error(loc, env,
                          Unbound_type_variable (Pprintast.tyvar_of_name name,
                                                 get_in_scope_names (),
                                                 Some Upstream_compatibility))))
      !used_variables;
    used_variables := TyVarMap.empty;
    fun () ->
      List.iter
        (function (loc, t1, t2) ->
          try unify env t1 t2 with Unify err ->
            raise (Error(loc, env, Type_mismatch err)))
        !r
end

(* Support for first-class modules. *)

let transl_modtype_longident = ref (fun _ -> assert false)
let transl_modtype = ref (fun _ -> assert false)
let check_package_with_type_constraints = ref (fun _ -> assert false)

let sort_constraints_no_duplicates loc env l =
  List.sort
    (fun (s1, _t1) (s2, _t2) ->
       if s1.txt = s2.txt then
         raise (Error (loc, env, Multiple_constraints_on_type s1.txt));
       compare s1.txt s2.txt)
    l

(* Translation of type expressions *)

let generalize_ctyp typ = generalize typ.ctyp_type

let strict_ident c = (c = '_' || c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z')

let validate_name = function
    None -> None
  | Some name as s ->
      if name <> "" && strict_ident name.[0] then s else None

let new_global_var ?name jkind =
  new_global_var ?name:(validate_name name) jkind
let newvar ?name jkind =
  newvar ?name:(validate_name name) jkind

let valid_tyvar_name name =
  name <> "" && name.[0] <> '_'

let transl_type_param_var env loc attrs name_opt
      (jkind : jkind_lr) jkind_annot =
  let tvar = Ttyp_var (name_opt, jkind_annot) in
  let name =
    match name_opt with
    | None -> "_"
    | Some name ->
      if not (valid_tyvar_name name) then
        raise (Error (loc, Env.empty, Invalid_variable_name ("'" ^ name)));
      if TyVarEnv.is_in_scope name then
        raise Already_bound;
      name
  in
  let ty = new_global_var ~name jkind in
  Option.iter (fun name -> TyVarEnv.add name ty jkind) name_opt;
  { ctyp_desc = tvar; ctyp_type = ty; ctyp_env = env;
    ctyp_loc = loc; ctyp_attributes = attrs }

let transl_type_param env path jkind_default styp =
  let loc = styp.ptyp_loc in
  let transl_jkind_and_annot_opt jkind_annot name =
    match jkind_annot with
    | None -> jkind_default, None
    | Some jkind_annot ->
      Jkind.of_annotation ~context:(Type_parameter (path, name)) jkind_annot,
      Some jkind_annot
  in
  let attrs = styp.ptyp_attributes in
  match styp.ptyp_desc with
    Ptyp_any jkind ->
      let name = None in
      let jkind, jkind_annot = transl_jkind_and_annot_opt jkind name in
      transl_type_param_var env loc attrs name jkind jkind_annot
  | Ptyp_var (name, jkind) ->
      let name = Some name in
      let jkind, jkind_annot = transl_jkind_and_annot_opt jkind name in
      transl_type_param_var env loc attrs name jkind jkind_annot
  | _ -> assert false

let transl_type_param env path jkind_default styp =
  (* Currently useless, since type parameters cannot hold attributes
     (but this could easily be lifted in the future). *)
  Builtin_attributes.warning_scope styp.ptyp_attributes
    (fun () -> transl_type_param env path jkind_default styp)

let get_type_param_jkind path styp =
  let of_annotation jkind name =
    let jkind =
      Jkind.of_annotation ~context:(Type_parameter (path, name)) jkind
    in
    jkind
  in
  match styp.ptyp_desc with
  | Ptyp_any (Some jkind) ->
      of_annotation jkind None
  | Ptyp_var (name, Some jkind) ->
      of_annotation jkind (Some name)
  | _ -> Jkind.of_new_legacy_sort ~why:(Unannotated_type_parameter path)

let get_type_param_name styp =
  (* We don't need to check for jkinds here, just to get the name. *)
  match styp.ptyp_desc with
  | Ptyp_any _ -> None
  | Ptyp_var (name, _) -> Some name
  | _ -> Misc.fatal_error "non-type-variable in get_type_param_name"

let rec extract_params styp =
  match styp.ptyp_desc with
  | Ptyp_arrow (l, a, r, ma, mr) ->
      let arg_mode = Typemode.transl_alloc_mode ma in
      let params, ret, ret_mode =
        match r.ptyp_desc with
        | Ptyp_arrow _ when not (Builtin_attributes.has_curry r.ptyp_attributes) ->
          extract_params r
        | _ -> [], r, Typemode.transl_alloc_mode mr
      in
      (l, arg_mode, a) :: params, ret, ret_mode
  | _ -> assert false

let check_arg_type styp =
  if not (Language_extension.is_enabled Polymorphic_parameters) then begin
    match styp.ptyp_desc with
    | Ptyp_poly _ ->
        raise (Error (styp.ptyp_loc, Env.empty,
                      Unsupported_extension Polymorphic_parameters))
    | _ -> ()
  end

let transl_label (label : Parsetree.arg_label)
    (arg_opt : Parsetree.core_type option) =
  match label, arg_opt with
  | Labelled l, Some { ptyp_desc = Ptyp_extension ({txt="call_pos"; _}, _); _}
      -> Position l
  | _, Some ({ ptyp_desc = Ptyp_extension ({txt="call_pos"; _}, _); _} as arg)
      -> raise (Error (arg.ptyp_loc, Env.empty, Invalid_label_for_call_pos label))
  | Labelled l, _ -> Labelled l
  | Optional l, _ -> Optional l
  | Nolabel, _ -> Nolabel

(* Parallel to [transl_label_from_expr]. *)
let transl_label_from_pat (label : Parsetree.arg_label)
    (pat : Parsetree.pattern) =
  match pat with
  (* We should only strip off the constraint node if the label translates
     to Position, as this means the type annotation is [%call_pos] and
     nothing more. *)
  | {ppat_desc = Ppat_constraint (inner_pat, ty, []); _} ->
      let label = transl_label label ty in
      let pat = if Btype.is_position label then inner_pat else pat in
      label, pat
  | _ -> transl_label label None, pat

(* Parallel to [transl_label_from_pat]. *)
let transl_label_from_expr (label : Parsetree.arg_label)
    (expr : Parsetree.expression) =
  match expr with
  | {pexp_desc = Pexp_constraint (inner_expr, ty, []); _} ->
      let label = transl_label label ty in
      let expr = if Btype.is_position label then inner_expr else expr in
      label, expr
  | _ -> transl_label label None, expr

let enrich_with_attributes attrs annotation_context =
  match Builtin_attributes.error_message_attr attrs with
  | Some msg -> Jkind.History.With_error_message (msg, annotation_context)
  | None -> annotation_context

let jkind_of_annotation annotation_context attrs jkind =
  Jkind.of_annotation ~context:(enrich_with_attributes attrs annotation_context)
    jkind

(* translate the ['a 'b ('c : immediate) .] part of a polytype,
   returning a [poly_univars] *)
let transl_bound_vars vars_jkinds =
  TyVarEnv.make_poly_univars_jkinds
    ~context:(fun v -> Univar ("'" ^ v)) vars_jkinds

(* Forward declaration (set in Typemod.type_open) *)
let type_open :
  (?used_slot:bool ref -> override_flag -> Env.t -> Location.t ->
   Longident.t loc -> Path.t * Env.t)
    ref =
  ref (fun ?used_slot:_ _ -> assert false)

let rec transl_type env ~policy ?(aliased=false) ~row_context mode styp =
  Builtin_attributes.warning_scope styp.ptyp_attributes
    (fun () -> transl_type_aux env ~policy ~aliased ~row_context mode styp)

and transl_type_aux env ~row_context ~aliased ~policy mode styp =
  let loc = styp.ptyp_loc in
  let ctyp ctyp_desc ctyp_type =
    { ctyp_desc; ctyp_type; ctyp_env = env;
      ctyp_loc = loc; ctyp_attributes = styp.ptyp_attributes }
  in
  match styp.ptyp_desc with
    Ptyp_any jkind ->
      let tjkind, tjkind_annot =
        match jkind with
        | None -> TyVarEnv.new_jkind ~is_named:false policy, None
        | Some jkind ->
            let tjkind =
              jkind_of_annotation (Type_wildcard loc)
                styp.ptyp_attributes jkind
            in
            tjkind, Some jkind
      in
      let ty = TyVarEnv.new_any_var loc env tjkind policy in
      ctyp (Ttyp_var (None, tjkind_annot)) ty
  | Ptyp_var (name, jkind) ->
      let desc, typ =
        transl_type_var env ~policy ~row_context
          styp.ptyp_attributes styp.ptyp_loc name jkind
      in
      ctyp desc typ
  | Ptyp_arrow _ ->
      let args, ret, ret_mode = extract_params styp in
      let rec loop acc_mode args =
        match args with
        | (l, arg_mode, arg) :: rest ->
          check_arg_type arg;
          let l = transl_label l (Some arg) in
          let arg_cty =
            if Btype.is_position l then
              ctyp Ttyp_call_pos (newconstr Predef.path_lexing_position [])
            else transl_type env ~policy ~row_context arg_mode arg
          in
          let acc_mode = curry_mode acc_mode arg_mode in
          let ret_mode =
            match rest with
            | [] -> ret_mode
            | _ :: _ -> acc_mode
          in
          let ret_cty = loop acc_mode rest in
          let arg_ty = arg_cty.ctyp_type in
          let arg_ty =
            if Btype.is_Tpoly arg_ty then arg_ty else newmono arg_ty
          in
          let arg_ty =
            if not (Btype.is_optional l) then arg_ty
            else begin
              if not (Btype.tpoly_is_mono arg_ty) then
                raise (Error (arg.ptyp_loc, env, Polymorphic_optional_param));
              newmono
                (newconstr Predef.path_option [Btype.tpoly_get_mono arg_ty])
            end
          in
          let arg_mode = Alloc.of_const arg_mode in
          let ret_mode = Alloc.of_const ret_mode in
          let arrow_desc = (l, arg_mode, ret_mode) in
          let ty =
            newty (Tarrow(arrow_desc, arg_ty, ret_cty.ctyp_type, commu_ok))
          in
          ctyp (Ttyp_arrow (l, arg_cty, ret_cty)) ty
        | [] -> transl_type env ~policy ~row_context ret_mode ret
      in
      loop mode args
  | Ptyp_tuple stl ->
    let desc, typ =
      transl_type_aux_tuple env ~loc ~policy ~row_context stl
    in
    ctyp desc typ
  | Ptyp_unboxed_tuple stl ->
    Language_extension.assert_enabled ~loc Layouts Language_extension.Stable;
    let tl =
      List.map
        (fun (label, t) ->
           label, transl_type env ~policy ~row_context Alloc.Const.legacy t)
        stl
    in
    let ctyp_type =
      newty (Tunboxed_tuple
               (List.map (fun (label, ctyp) -> label, ctyp.ctyp_type) tl))
    in
    ctyp (Ttyp_unboxed_tuple tl) ctyp_type
  | Ptyp_constr(lid, stl) ->
      let (path, decl) = Env.lookup_type ~loc:lid.loc lid.txt env in
      let stl =
        match stl with
        | [ {ptyp_desc=Ptyp_any None} as t ] when decl.type_arity > 1 ->
            List.map (fun _ -> t) decl.type_params
        | _ -> stl
      in
      if List.length stl <> decl.type_arity then
        raise(Error(styp.ptyp_loc, env,
                    Type_arity_mismatch(lid.txt, decl.type_arity,
                                        List.length stl)));
      let args =
        List.map (transl_type env ~policy ~row_context Alloc.Const.legacy) stl
      in
      let params = instance_list decl.type_params in
      let unify_param =
        match decl.type_manifest with
          None -> unify_var
        | Some ty ->
            if get_level ty = Btype.generic_level then unify_var else unify
      in
      let arity = List.length params in
      List.iteri
        (fun idx ((sty, cty), ty') ->
           begin match Types.get_desc ty' with
           | Tvar {jkind; _} when Jkind.History.is_imported jkind ->
             (* In case of a Tvar with imported jkind history, we can improve
                the jkind reason using the in scope [path] to the parent type.

                Basic benchmarking suggests this change doesn't have that big
                of a performance impact: compiling [types.ml] resulted in 13k
                extra alloc (~0.01% increase) and building the core library had
                no statistically significant increase in build time. *)
             let reason = Jkind.History.Imported_type_argument
                            {parent_path = path; position = idx + 1; arity} in
             Types.set_var_jkind ty' (Jkind.History.update_reason jkind reason)
           | _ -> ()
           end;
           try unify_param env ty' cty.ctyp_type with Unify err ->
             let err = Errortrace.swap_unification_error err in
             raise (Error(sty.ptyp_loc, env, Type_mismatch err))
        )
        (List.combine (List.combine stl args) params);
      let constr =
        newconstr path (List.map (fun ctyp -> ctyp.ctyp_type) args) in
      ctyp (Ttyp_constr (path, lid, args)) constr
  | Ptyp_object (fields, o) ->
      let ty, fields = transl_fields env ~policy ~row_context o fields in
      ctyp (Ttyp_object (fields, o)) (newobj ty)
  | Ptyp_class(lid, stl) ->
      let (path, decl) =
        match Env.lookup_cltype ~loc:lid.loc lid.txt env with
        | (path, decl) -> (path, decl.clty_hash_type)
        (* Raise a different error if it matches the name of an unboxed type *)
        | exception
            (Env.Error (Lookup_error (_, _, Unbound_cltype _)) as exn)
          ->
            let unboxed_lid : Longident.t =
              match lid.txt with
              | Lident s -> Lident (s ^ "#")
              | Ldot (l, s) -> Ldot (l, s ^ "#")
              | Lapply _ -> fatal_error "Typetexp.transl_type"
            in
            match Env.find_type_by_name unboxed_lid env with
            | exception Not_found -> raise exn
            | (_ : _ * _) ->
                raise (Error (styp.ptyp_loc, env, Did_you_mean_unboxed lid.txt))
      in
      if List.length stl <> decl.type_arity then
        raise(Error(styp.ptyp_loc, env,
                    Type_arity_mismatch(lid.txt, decl.type_arity,
                                        List.length stl)));
      let args =
        List.map (transl_type env ~policy ~row_context Alloc.Const.legacy) stl
      in
      let body = Option.get decl.type_manifest in
      let (params, body) = instance_parameterized_type decl.type_params body in
      List.iter2
        (fun (sty, cty) ty' ->
           try unify_var env ty' cty.ctyp_type with Unify err ->
             let err = Errortrace.swap_unification_error err in
             raise (Error(sty.ptyp_loc, env, Type_mismatch err))
        )
        (List.combine stl args) params;
      let ty_args = List.map (fun ctyp -> ctyp.ctyp_type) args in
      let ty = Ctype.apply ~use_current_level:true env params body ty_args in
      let ty = match get_desc ty with
        | Tobject (fi, _) ->
            let _, tv = flatten_fields fi in
            TyVarEnv.add_pre_univar tv policy;
            ty
        | _ ->
            assert false
      in
      ctyp (Ttyp_class (path, lid, args)) ty
  | Ptyp_alias(st, alias, jkind) ->
    let desc, typ =
      transl_type_alias env ~policy ~row_context
        mode styp.ptyp_attributes loc st alias jkind
    in
    ctyp desc typ
  | Ptyp_variant(fields, closed, present) ->
      let name = ref None in
      let mkfield l f =
        newty (Tvariant (create_row ~fields:[l,f]
                           ~more:(newvar (Jkind.Builtin.value ~why:Row_variable))
                           ~closed:true ~fixed:None ~name:None)) in
      let hfields = Hashtbl.create 17 in
      let add_typed_field loc l f =
        let h = Btype.hash_variant l in
        try
          let (l',f') = Hashtbl.find hfields h in
          (* Check for tag conflicts *)
          if l <> l' then raise(Error(styp.ptyp_loc, env, Variant_tags(l, l')));
          let ty = mkfield l f and ty' = mkfield l f' in
          if is_equal env false [ty] [ty'] then () else
          try unify env ty ty'
          with Unify _trace ->
            raise(Error(loc, env, Constructor_mismatch (ty,ty')))
        with Not_found ->
          Hashtbl.add hfields h (l,f)
      in
      let add_field row_context field =
        let rf_loc = field.prf_loc in
        let rf_attributes = field.prf_attributes in
        let rf_desc = match field.prf_desc with
        | Rtag (l, c, stl) ->
            name := None;
            let tl =
              Builtin_attributes.warning_scope rf_attributes
                (fun () ->
                   List.map
                     (transl_type env ~policy ~row_context Alloc.Const.legacy)
                     stl)
            in
            List.iter (fun {ctyp_type; ctyp_loc} ->
              (* CR layouts: at some point we'll allow different jkinds in
                 polymorphic variants. *)
              match
                constrain_type_jkind env ctyp_type
                  (Jkind.Builtin.value_or_null ~why:Polymorphic_variant_field)
              with
              | Ok _ -> ()
              | Error e ->
                raise (Error(ctyp_loc, env,
                             Non_value {vloc = Poly_variant; err = e;
                                        typ = ctyp_type})))
              tl;
            let f = match present with
              Some present when not (List.mem l.txt present) ->
                let ty_tl = List.map (fun cty -> cty.ctyp_type) tl in
                rf_either ty_tl ~no_arg:c ~matched:false
            | _ ->
                if List.length stl > 1 || c && stl <> [] then
                  raise(Error(styp.ptyp_loc, env,
                              Present_has_conjunction l.txt));
                match tl with [] -> rf_present None
                | st :: _ -> rf_present (Some st.ctyp_type)
            in
            add_typed_field styp.ptyp_loc l.txt f;
              Ttag (l,c,tl)
        | Rinherit sty ->
            let cty =
              transl_type env ~policy ~row_context Alloc.Const.legacy sty
            in
            let ty = cty.ctyp_type in
            let nm =
              match get_desc cty.ctyp_type with
                Tconstr(p, tl, _) -> Some(p, tl)
              | _                 -> None
            in
            name := if Hashtbl.length hfields <> 0 then None else nm;
            let fl = match get_desc (expand_head env cty.ctyp_type), nm with
              Tvariant row, _ when Btype.static_row row ->
                row_fields row
            | Tvar _, Some(p, _) ->
                raise(Error(sty.ptyp_loc, env, Undefined_type_constructor p))
            | _ ->
                raise(Error(sty.ptyp_loc, env, Not_a_variant ty))
            in
            List.iter
              (fun (l, f) ->
                let f = match present with
                  Some present when not (List.mem l present) ->
                    begin match row_field_repr f with
                      Rpresent oty -> rf_either_of oty
                    | _ -> assert false
                    end
                | _ -> f
                in
                add_typed_field sty.ptyp_loc l f)
              fl;
              Tinherit cty
        in
        { rf_desc; rf_loc; rf_attributes; }
      in
      let more_slot = ref None in
      let row_context =
        if aliased then row_context else more_slot :: row_context
      in
      let tfields = List.map (add_field row_context) fields in
      let fields = List.rev (Hashtbl.fold (fun _ p l -> p :: l) hfields []) in
      begin match present with None -> ()
      | Some present ->
          List.iter
            (fun l -> if not (List.mem_assoc l fields) then
              raise(Error(styp.ptyp_loc, env, Present_has_no_type l)))
            present
      end;
      let name = !name in
      let make_row more =
        create_row ~fields ~more ~closed:(closed = Asttypes.Closed)
          ~fixed:None ~name
      in
      let more =
        if Btype.static_row
             (make_row (newvar (Jkind.Builtin.value ~why:Row_variable)))
        then newty Tnil
        else TyVarEnv.new_var (Jkind.Builtin.value ~why:Row_variable) policy
      in
      more_slot := Some more;
      let ty = newty (Tvariant (make_row more)) in
      ctyp (Ttyp_variant (tfields, closed, present)) ty
  | Ptyp_poly(vars, st) ->
      let desc, typ =
        transl_type_poly env ~policy ~row_context mode styp.ptyp_loc
          vars st
      in
      ctyp desc typ
  | Ptyp_package (p, l) ->
    (* CR layouts: right now we're doing a real gross hack where we demand
       everything in a package type with constraint be value.

       An alternative is to walk into the constrained module, using the
       longidents, and find the actual things that need jkind checking.
       See [Typemod.package_constraints_sig] for code that does a
       similar traversal from a longident.
    *)
    (* CR layouts: and in the long term, rewrite all of this to eliminate
       the [create_package_mty] hack that constructs fake source code. *)
      let loc = styp.ptyp_loc in
      let l = sort_constraints_no_duplicates loc env l in
      let mty = Ast_helper.Mty.mk ~loc (Pmty_ident p) in
      let mty = TyVarEnv.with_local_scope (fun () -> !transl_modtype env mty) in
      let ptys =
        List.map (fun (s, pty) ->
          s, transl_type env ~policy ~row_context Alloc.Const.legacy pty
        ) l
      in
      let mty =
        if ptys <> [] then
          !check_package_with_type_constraints loc env mty.mty_type ptys
        else mty.mty_type
      in
      let path = !transl_modtype_longident loc env p.txt in
      let ty = newty (Tpackage (path,
                       List.map (fun (s, cty) -> (s.txt, cty.ctyp_type)) ptys))
      in
      ctyp (Ttyp_package {
            pack_path = path;
            pack_type = mty;
            pack_fields = ptys;
            pack_txt = p;
           }) ty
  | Ptyp_open (mod_ident, t) ->
      let path, new_env =
        !type_open Asttypes.Fresh env loc mod_ident
      in
      let cty = transl_type new_env ~policy ~row_context mode t in
      ctyp (Ttyp_open (path, mod_ident, cty)) cty.ctyp_type
  | Ptyp_of_kind jkind ->
    let tjkind = jkind_of_annotation (Type_of_kind loc) styp.ptyp_attributes jkind in
    let ty = newty (Tof_kind tjkind) in
    ctyp (Ttyp_of_kind jkind) ty
  | Ptyp_extension ext ->
      raise (Error_forward (Builtin_attributes.error_of_extension ext))

and transl_type_var env ~policy ~row_context attrs loc name jkind_annot_opt =
  let print_name = "'" ^ name in
  if not (valid_tyvar_name name) then
    raise (Error (loc, env, Invalid_variable_name print_name));
  let of_annot = jkind_of_annotation (Type_variable print_name) attrs in
  let ty = try
      TyVarEnv.lookup_local ~row_context name
    with Not_found ->
      let jkind =
        (* See Note [Global type variables] *)
        try TyVarEnv.lookup_global_jkind name
        with Not_found -> TyVarEnv.new_jkind ~is_named:true policy
      in
      let ty = TyVarEnv.new_var ~name jkind policy in
      TyVarEnv.remember_used name ty loc;
      ty
  in
  let jkind_annot =
    match jkind_annot_opt with
    | None -> None
    | Some jkind_annot ->
      let jkind = of_annot jkind_annot in
      match constrain_type_jkind env ty jkind with
      | Ok () -> Some jkind_annot
      | Error err ->
          raise (Error(jkind_annot.pjkind_loc, env, Bad_jkind_annot (ty, err)))
  in
  Ttyp_var (Some name, jkind_annot), ty

and transl_type_poly env ~policy ~row_context mode loc vars st =
  let typed_vars, new_univars, cty =
    with_local_level begin fun () ->
      let new_univars = transl_bound_vars vars in
      let typed_vars = TyVarEnv.ttyp_poly_arg new_univars in
      let cty = TyVarEnv.with_univars new_univars begin fun () ->
        transl_type env ~policy ~row_context mode st
      end in
      (typed_vars, new_univars, cty)
    end
      ~post:(fun (_,_,cty) -> generalize_ctyp cty)
  in
  let ty = cty.ctyp_type in
  let ty_list = TyVarEnv.check_poly_univars env loc new_univars in
  let ty_list = List.filter (fun v -> deep_occur v ty) ty_list in
  let ty' = Btype.newgenty (Tpoly(ty, ty_list)) in
  unify_var env (newvar (Jkind.Builtin.any ~why:Dummy_jkind)) ty';
  Ttyp_poly (typed_vars, cty), ty'

and transl_type_alias env ~row_context ~policy mode attrs styp_loc styp name_opt
      jkind_annot_opt =
  let cty, jkind_annot = match name_opt with
    | Some { txt = alias; loc = alias_loc } ->
      begin try
        let t = TyVarEnv.lookup_local ~row_context alias in
        let cty =
          transl_type env ~policy ~aliased:true ~row_context mode styp
        in
        begin try unify_var env t cty.ctyp_type with Unify err ->
          let err = Errortrace.swap_unification_error err in
          raise(Error(alias_loc, env, Alias_type_mismatch err))
        end;
        let jkind_annot = match jkind_annot_opt with
        | None -> None
        | Some jkind_annot ->
          let jkind =
            jkind_of_annotation (Type_variable ("'" ^ alias)) attrs jkind_annot
          in
          begin match constrain_type_jkind env t jkind with
          | Ok () -> ()
          | Error err ->
            raise (Error(jkind_annot.pjkind_loc, env, Bad_jkind_annot(t, err)))
          end;
          Some jkind_annot
        in
        cty, jkind_annot
      with Not_found ->
        let t, ty, jkind_annot =
          with_local_level_if_principal begin fun () ->
            let jkind, jkind_annot =
              match jkind_annot_opt with
              | None -> Jkind.Builtin.any ~why:Dummy_jkind, None
              | Some jkind_annot ->
                let jkind =
                  jkind_of_annotation (Type_variable ("'" ^ alias)) attrs jkind_annot
                in
                jkind, Some jkind_annot
            in
            let t = newvar jkind in
            (* Use the whole location, which is used by [Type_mismatch]. *)
            TyVarEnv.remember_used alias t styp_loc;
            let ty = transl_type env ~policy ~row_context mode styp in
            begin try unify_var env t ty.ctyp_type with Unify err ->
              let err = Errortrace.swap_unification_error err in
              raise(Error(alias_loc, env, Alias_type_mismatch err))
            end;
            (t, ty, jkind_annot)
          end
          ~post: (fun (t, _, _) -> generalize_structure t)
        in
        let t = instance t in
        let px = Btype.proxy t in
        begin match get_desc px with
        | Tvar { name = None; jkind } ->
           set_type_desc px (Tvar { name = Some alias; jkind })
        | Tunivar { name = None; jkind } ->
           set_type_desc px (Tunivar {name = Some alias; jkind})
        | _ -> ()
        end;
        { ty with ctyp_type = t }, jkind_annot
      end
    | None ->
      let cty = transl_type env ~policy ~row_context mode styp in
      let cty_expr = cty.ctyp_type in
      let jkind_annot = match jkind_annot_opt with
        | None -> Misc.fatal_error "anonymous alias without layout annotation"
        | Some jkind_annot -> jkind_annot
      in
      let jkind =
        jkind_of_annotation (Type_wildcard jkind_annot.pjkind_loc)
          attrs jkind_annot
      in
      begin match constrain_type_jkind env cty_expr jkind with
      | Ok () -> ()
      | Error err ->
        raise (Error(jkind_annot.pjkind_loc, env,
                     Bad_jkind_annot(cty_expr, err)))
      end;
      cty, Some jkind_annot
  in
  Ttyp_alias (cty, name_opt, jkind_annot),
  cty.ctyp_type

and transl_type_aux_tuple env ~loc ~policy ~row_context stl =
  assert (List.length stl >= 2);
  let ctys =
    List.map
      (fun (label, t) ->
         Option.iter (fun _ ->
             Language_extension.assert_enabled ~loc Labeled_tuples ())
           label;
         label, transl_type env ~policy ~row_context Alloc.Const.legacy t)
      stl
  in
  List.iter (fun (_, {ctyp_type; ctyp_loc}) ->
    (* CR layouts v5: remove value requirement *)
    match
      constrain_type_jkind env ctyp_type (Jkind.Builtin.value_or_null ~why:Tuple_element)
    with
    | Ok _ -> ()
    | Error e ->
      raise (Error(ctyp_loc, env,
                   Non_value {vloc = Tuple; err = e; typ = ctyp_type})))
    ctys;
  let ctyp_type =
    newty (Ttuple (List.map (fun (label, ctyp) -> label, ctyp.ctyp_type) ctys))
  in
  Ttyp_tuple ctys, ctyp_type

and transl_fields env ~policy ~row_context o fields =
  let hfields = Hashtbl.create 17 in
  let add_typed_field loc l ty =
    try
      let ty' = Hashtbl.find hfields l in
      if is_equal env false [ty] [ty'] then () else
        try unify env ty ty'
        with Unify _trace ->
          raise(Error(loc, env, Method_mismatch (l, ty, ty')))
    with Not_found ->
      Hashtbl.add hfields l ty in
  let add_field {pof_desc; pof_loc; pof_attributes;} =
    let of_loc = pof_loc in
    let of_attributes = pof_attributes in
    let of_desc = match pof_desc with
    | Otag (s, ty1) -> begin
        let ty1 =
          Builtin_attributes.warning_scope of_attributes
            (fun () -> transl_type env ~policy ~row_context Alloc.Const.legacy
                (Ast_helper.Typ.force_poly ty1))
        in
        begin
          match
            constrain_type_jkind
              env ty1.ctyp_type (Jkind.Builtin.value ~why:Object_field)
          with
          | Ok _ -> ()
          | Error e ->
            raise (Error(of_loc, env,
                         Non_value {vloc = Object_field; err = e;
                                    typ = ty1.ctyp_type}))
        end;
        let field = OTtag (s, ty1) in
        add_typed_field ty1.ctyp_loc s.txt ty1.ctyp_type;
        field
      end
    | Oinherit sty -> begin
        let cty = transl_type env ~policy ~row_context Alloc.Const.legacy sty in
        let nm =
          match get_desc cty.ctyp_type with
            Tconstr(p, _, _) -> Some p
          | _                -> None in
        let t = expand_head env cty.ctyp_type in
        match get_desc t, nm with
          Tobject (tf, _), _
          when (match get_desc tf with Tfield _ | Tnil -> true | _ -> false) ->
            begin
              if opened_object t then
                raise (Error (sty.ptyp_loc, env, Opened_object nm));
              let rec iter_add ty =
                match get_desc ty with
                | Tfield (s, _k, ty1, ty2) ->
                    add_typed_field sty.ptyp_loc s ty1;
                    iter_add ty2
                | Tnil -> ()
                | _ -> assert false
              in
              iter_add tf;
              OTinherit cty
            end
        | Tvar _, Some p ->
            raise (Error (sty.ptyp_loc, env, Undefined_type_constructor p))
        | _ -> raise (Error (sty.ptyp_loc, env, Not_an_object t))
      end in
    { of_desc; of_loc; of_attributes; }
  in
  let object_fields = List.map add_field fields in
  let fields = Hashtbl.fold (fun s ty l -> (s, ty) :: l) hfields [] in
  let ty_init =
     match o with
     | Asttypes.Closed -> newty Tnil
     | Asttypes.Open ->
        TyVarEnv.new_var (Jkind.Builtin.value ~why:Row_variable) policy
  in
  let ty = List.fold_left (fun ty (s, ty') ->
      newty (Tfield (s, field_public, ty', ty))) ty_init fields in
  ty, object_fields

(* Make the rows "fixed" in this type, to make universal check easier *)
let rec make_fixed_univars ty =
  if Btype.try_mark_node ty then
    begin match get_desc ty with
    | Tvariant row ->
        let Row {fields; more; name; closed} = row_repr row in
        if Btype.is_Tunivar more then
          let fields =
            List.map
              (fun (s,f as p) -> match row_field_repr f with
                Reither (no_arg, tl, _m) ->
                  s, rf_either tl ~use_ext_of:f ~no_arg ~matched:true
              | _ -> p)
              fields
          in
          set_type_desc ty
            (Tvariant
               (create_row ~fields ~more ~name ~closed
                  ~fixed:(Some (Univar more))));
        Btype.iter_row make_fixed_univars row
    | _ ->
        Btype.iter_type_expr make_fixed_univars ty
    end

let transl_type env policy mode styp =
  transl_type env ~policy ~row_context:[] mode styp

let make_fixed_univars ty =
  make_fixed_univars ty;
  Btype.unmark_type ty

let transl_simple_type_impl env ~new_var_jkind ?univars ~policy mode styp =
  TyVarEnv.reset_locals ?univars ();
  let policy = TyVarEnv.make_policy policy new_var_jkind in
  let typ = transl_type env policy mode styp in
  TyVarEnv.globalize_used_variables policy env ();
  make_fixed_univars typ.ctyp_type;
  typ

let transl_simple_type env ~new_var_jkind ?univars ~closed mode styp =
  let policy = if closed then Closed else Open in
  transl_simple_type_impl env ~new_var_jkind ?univars ~policy mode styp

let transl_simple_type_univars env styp =
  TyVarEnv.reset_locals ();
  let typ, univs =
    TyVarEnv.collect_univars begin fun () ->
      with_local_level ~post:generalize_ctyp begin fun () ->
        let policy = TyVarEnv.univars_policy in
        let typ = transl_type env policy Alloc.Const.legacy styp in
        TyVarEnv.globalize_used_variables policy env ();
        typ
      end
  end in
  make_fixed_univars typ.ctyp_type;
    { typ with ctyp_type =
        instance (Btype.newgenty (Tpoly (typ.ctyp_type, univs))) }

let transl_simple_type_delayed env mode styp =
  TyVarEnv.reset_locals ();
  let typ, force =
    with_local_level begin fun () ->
      let policy = TyVarEnv.make_policy Open Any in
      let typ = transl_type env policy mode styp in
      make_fixed_univars typ.ctyp_type;
      (* This brings the used variables to the global level, but doesn't link
         them to their other occurrences just yet. This will be done when
         [force] is  called. *)
      let force = TyVarEnv.globalize_used_variables policy env in
      (typ, force)
    end
    (* Generalize everything except the variables that were just globalized. *)
    ~post:(fun (typ,_) -> generalize_ctyp typ)
  in
  (typ, instance typ.ctyp_type, force)

let transl_type_scheme_mono env styp =
  let typ =
    with_local_level begin fun () ->
      TyVarEnv.reset ();
      transl_simple_type ~new_var_jkind:Sort env ~closed:false Alloc.Const.legacy styp
    end
    ~post:generalize_ctyp
  in
  (* This next line is very important: it stops [val] and [external]
     declarations from having undefaulted jkind variables. Without
     this line, we might accidentally export a jkind-flexible definition
     from a compilation unit, which would lead to miscompilation. *)
  remove_mode_and_jkind_variables typ.ctyp_type;
  typ

let transl_type_scheme_poly env attrs loc vars inner_type =
  let typed_vars, univars, typ =
    with_local_level begin fun () ->
      TyVarEnv.reset ();
      let univars = transl_bound_vars vars in
      let typed_vars = TyVarEnv.ttyp_poly_arg univars in
      let typ =
        if Language_extension.erasable_extensions_only () then
          transl_simple_type_impl ~new_var_jkind:Sort env ~univars
            ~policy:Closed_for_upstream_compatibility Alloc.Const.legacy
            inner_type
        else
          transl_simple_type_impl ~new_var_jkind:Sort env ~univars ~policy:Open
            Alloc.Const.legacy inner_type
      in
      (typed_vars, univars, typ)
    end
    ~post:(fun (_,_,typ) -> generalize_ctyp typ)
  in
  let _ : _ list = TyVarEnv.instance_poly_univars env loc univars in
  { ctyp_desc = Ttyp_poly (typed_vars, typ);
    ctyp_type = typ.ctyp_type;
    ctyp_env = env;
    ctyp_loc = loc;
    ctyp_attributes = attrs }

let transl_type_scheme env styp =
  match styp.ptyp_desc with
  | Ptyp_poly (vars, st) ->
    transl_type_scheme_poly env styp.ptyp_attributes
      styp.ptyp_loc vars st
  | _ ->
    transl_type_scheme_mono env styp

(* Error report *)

open Format
open Printtyp
module Style = Misc.Style
let pp_tag ppf t = Format.fprintf ppf "`%s" t

let report_unbound_variable_reason ppf = function
  | Some Upstream_compatibility ->
      fprintf ppf "@.Hint: Explicit quantification requires quantifying all \
                   type variables for compatibility with upstream OCaml.\n\
                   Enable non-erasable extensions to disable this check."
  | None -> ()

let report_error env ppf =
  function
  | Unbound_type_variable (name, in_scope_names, reason) ->
    fprintf ppf "The type variable %a is unbound in this type declaration.@ %a"
      Style.inline_code name
      did_you_mean (fun () -> Misc.spellcheck in_scope_names name );
    report_unbound_variable_reason ppf reason
  | No_type_wildcards reason ->
      fprintf ppf "A type wildcard %a is not allowed in this type declaration."
        Style.inline_code "_";
      report_unbound_variable_reason ppf reason
  | Undefined_type_constructor p ->
    fprintf ppf "The type constructor@ %a@ is not yet completely defined"
      (Style.as_inline_code path) p
  | Type_arity_mismatch(lid, expected, provided) ->
    fprintf ppf
      "@[The type constructor %a@ expects %i argument(s),@ \
        but is here applied to %i argument(s)@]"
      (Style.as_inline_code longident) lid expected provided
  | Bound_type_variable name ->
      fprintf ppf "Already bound type parameter %a"
        (Style.as_inline_code Pprintast.tyvar) name
  | Recursive_type ->
    fprintf ppf "This type is recursive"
  | Type_mismatch trace ->
      Printtyp.report_unification_error ppf Env.empty trace
        (function ppf ->
           fprintf ppf "This type")
        (function ppf ->
           fprintf ppf "should be an instance of type")
  | Alias_type_mismatch trace ->
      Printtyp.report_unification_error ppf Env.empty trace
        (function ppf ->
           fprintf ppf "This alias is bound to type")
        (function ppf ->
           fprintf ppf "but is used as an instance of type")
  | Present_has_conjunction l ->
      fprintf ppf "The present constructor %a has a conjunctive type"
        Style.inline_code l
  | Present_has_no_type l ->
      fprintf ppf
        "@[<v>@[The constructor %a is missing from the upper bound@ \
         (between %a@ and %a)@ of this polymorphic variant@ \
         but is present in@ its lower bound (after %a).@]@,\
         @[@{<hint>Hint@}: Either add %a in the upper bound,@ \
         or remove it@ from the lower bound.@]@]"
        (Style.as_inline_code pp_tag) l
        Style.inline_code "<"
        Style.inline_code ">"
        Style.inline_code ">"
        (Style.as_inline_code pp_tag) l
  | Constructor_mismatch (ty, ty') ->
      let pp_type ppf ty = Style.as_inline_code !Oprint.out_type ppf ty in
      wrap_printing_env ~error:true env (fun ()  ->
        Printtyp.prepare_for_printing [ty; ty'];
        fprintf ppf "@[<hov>%s %a@ %s@ %a@]"
          "This variant type contains a constructor"
          pp_type (tree_of_typexp Type ty)
          "which should be"
          pp_type (tree_of_typexp Type ty'))
  | Not_a_variant ty ->
      fprintf ppf
        "@[The type %a@ does not expand to a polymorphic variant type@]"
        (Style.as_inline_code Printtyp.type_expr) ty;
      begin match get_desc ty with
        | Tvar { name = Some s } ->
           (* PR#7012: help the user that wrote 'Foo instead of `Foo *)
           Misc.did_you_mean ppf (fun () -> ["`" ^ s])
        | _ -> ()
      end
  | Variant_tags (lab1, lab2) ->
      fprintf ppf
        "@[Variant tags %a@ and %a have the same hash value.@ %s@]"
        (Style.as_inline_code pp_tag) lab1
        (Style.as_inline_code pp_tag) lab2
        "Change one of them."
  | Invalid_variable_name name ->
      fprintf ppf "The type variable name %a is not allowed in programs"
        Style.inline_code name
  | Cannot_quantify (name, reason) ->
      fprintf ppf
        "@[<hov>The universal type variable %a cannot be generalized:@ "
        (Style.as_inline_code Pprintast.tyvar) name;
      begin match reason with
      | Unified v ->
        fprintf ppf "it is bound to@ %a"
          (Style.as_inline_code Printtyp.type_expr) v;
      | Univar ->
        fprintf ppf "it is already bound to another variable"
      | Scope_escape ->
        fprintf ppf "it escapes its scope"
      end;
      fprintf ppf ".@]";
  | Bad_univar_jkind { name; jkind_info; inferred_jkind } ->
      fprintf ppf
        "@[<hov>The universal type variable %a was %s to have kind %a.@;%a@]"
        Pprintast.tyvar name
        (if jkind_info.defaulted then "defaulted" else "declared")
        Jkind.format jkind_info.original_jkind
        (Jkind.format_history ~intro:(
          dprintf "But it was inferred to have %t"
            (fun ppf -> let desc = Jkind.get inferred_jkind in
              match desc.layout with
              | Sort (Var _) -> fprintf ppf "a representable kind"
              | Sort (Base _) | Any | Product _ ->
                fprintf ppf "kind %a" Jkind.format
                  inferred_jkind)))
        inferred_jkind
  | Multiple_constraints_on_type s ->
      fprintf ppf "Multiple constraints for type %a"
        (Style.as_inline_code longident) s
  | Method_mismatch (l, ty, ty') ->
      wrap_printing_env ~error:true env (fun ()  ->
        fprintf ppf "@[<hov>Method %a has type %a,@ which should be %a@]"
          Style.inline_code l
          (Style.as_inline_code Printtyp.type_expr) ty
          (Style.as_inline_code Printtyp.type_expr) ty')
  | Opened_object nm ->
      fprintf ppf
        "Illegal open object type%a"
        (fun ppf -> function
             Some p -> fprintf ppf "@ %a" (Style.as_inline_code path) p
           | None -> fprintf ppf "") nm
  | Not_an_object ty ->
      fprintf ppf "@[The type %a@ is not an object type@]"
        (Style.as_inline_code Printtyp.type_expr) ty
  | Unsupported_extension ext ->
      let ext = Language_extension.to_string ext in
      fprintf ppf "@[The %s extension is disabled@ \
                   To enable it, pass the '-extension %s' flag@]" ext ext
  | Polymorphic_optional_param ->
      fprintf ppf "@[Optional parameters cannot be polymorphic@]"
  | Non_value {vloc; typ; err} ->
    let s =
      match vloc with
      | Tuple -> "Tuple element"
      | Poly_variant -> "Polymorphic variant constructor argument"
      | Object_field -> "Object field"
    in
    fprintf ppf "@[%s types must have layout value.@ %a@]"
      s (Jkind.Violation.report_with_offender
           ~offender:(fun ppf ->
               Style.as_inline_code Printtyp.type_expr ppf typ)) err
  | Non_sort {vloc; typ; err} ->
    let s =
      match vloc with
      | Fun_arg -> "Function argument"
      | Fun_ret -> "Function return"
    in
    fprintf ppf "@[%s types must have a representable layout.@ %a@]"
      s (Jkind.Violation.report_with_offender
           ~offender:(fun ppf ->
               Style.as_inline_code Printtyp.type_expr ppf typ)) err
  | Bad_jkind_annot(ty, violation) ->
    fprintf ppf "@[<b 2>Bad layout annotation:@ %a@]"
      (Jkind.Violation.report_with_offender
         ~offender:(fun ppf ->
             Style.as_inline_code Printtyp.type_expr ppf ty)) violation
  | Did_you_mean_unboxed lid ->
    fprintf ppf "@[%a isn't a class type.@ \
                 Did you mean the unboxed type %a?@]"
      (Style.as_inline_code longident) lid
      (Style.as_inline_code (fun ppf lid -> fprintf ppf "%a#" longident lid)) lid
  | Invalid_label_for_call_pos arg_label ->
      fprintf ppf "A position argument must not be %s."
        (match arg_label with
        | Nolabel -> "unlabelled"
        | Optional _ -> "optional"
        | Labelled _ -> assert false )

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, env, err) ->
        Some (Location.error_of_printer ~loc (report_error env) err)
      | Error_forward err ->
        Some err
      | _ ->
        None
    )
