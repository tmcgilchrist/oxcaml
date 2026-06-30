(* TEST
 setup-ocamlc.byte-build-env;
 flags = "-dshape -shape-format debugging-shapes -dno-unique-ids";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Under [-shape-format debugging-shapes], a class is decomposed into a
   structured object shape carrying its methods, instance variables (with
   mutability) and inherited parents. *)

class point (x0 : int) = object
  val mutable x = x0
  method get_x = x
  method move dx = x <- x + dx
end

class colored_point x0 (c : string) = object
  inherit point x0
  val color = c
  method get_color = color
end
