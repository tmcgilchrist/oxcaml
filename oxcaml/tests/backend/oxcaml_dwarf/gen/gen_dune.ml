let () =
  (* Platform guards.

     Scalar (non-SIMD) tests run on x86-64 Linux (the reference platform) and on
     arm64 macOS. [test_simd_dwarf] additionally depends on the x86-only
     [simd_stubs.c] and on AVX2 codegen, so it stays x86-64 / non-macOS. *)
  let amd64_linux =
    {|(and (= %{architecture} "amd64") (<> %{system} "macosx"))|}
  in
  let arm64_macos =
    {|(and (= %{architecture} "arm64") (= %{system} "macosx"))|}
  in
  let portable_plat = {| (or |} ^ amd64_linux ^ {| |} ^ arm64_macos ^ {|)|} in
  let simd_plat = {| |} ^ amd64_linux in
  let mk_enabled_if plat =
    {|(enabled_if (and (= %{context_name} "main")|} ^ plat ^ {|))|}
  in
  let mk_enabled_if_with_lldb plat =
    {|(enabled_if
  (and
   (= %{context_name} "main")|} ^ plat
    ^ {|
   (<> %{env:OXCAML_LLDB=} "")))|}
  in
  let mk_enabled_if_without_lldb plat =
    {|(enabled_if
  (and
   (= %{context_name} "main")|} ^ plat
    ^ {|
   (= %{env:OXCAML_LLDB=} "")))|}
  in
  (* [-function-sections] is unsupported on Mach-O (OCaml's configure forces it
     off), so select it via a generated sexp fragment: present everywhere except
     macOS. Emitting it through a dune variable keeps [dune.inc] byte-identical
     across platforms. *)
  print_string
    {|(rule
 (enabled_if (<> %{system} "macosx"))
 (action
  (with-stdout-to function_sections_flags.sexp (echo "(-function-sections)"))))

(rule
 (enabled_if (= %{system} "macosx"))
 (action (with-stdout-to function_sections_flags.sexp (echo "()"))))

|};
  let buf = Buffer.create 1000 in
  (* Function to generate rules for executable tests that produce output *)
  let print_dwarf_test ?(simd = false) name =
    let plat = if simd then simd_plat else portable_plat in
    (* Every test's output embeds mangled symbol names, so all are
       scheme-dependent. Select the expected output by the active scheme; the
       executable below is built with the matching [-name-mangling-scheme] so a
       single box can produce either baseline by toggling the env var. *)
    let mangling_ext = ".%{env:OXCAML_NAME_MANGLING=flat}" in
    (* Per-platform [runtest-dwarf] diff stanzas. The expected source file is
       named directly (not via a copy) so [dune promote] can write back to
       it. *)
    let variants =
      if simd
      then [amd64_linux, "linux.amd64"]
      else [amd64_linux, "linux.amd64"; arm64_macos, "macos.arm64"]
    in
    let diff_rule (guard, suffix) =
      let expected = name ^ "." ^ suffix ^ mangling_ext ^ ".output" in
      {|(rule
 (alias runtest-dwarf)
 (enabled_if (and (= %{context_name} "main") |}
      ^ guard ^ {|))
 (deps |} ^ expected ^ {| |} ^ name
      ^ {|.output.corrected)
 (action
  (diff |} ^ expected ^ {| |} ^ name
      ^ {|.output.corrected)))
|}
    in
    let diff_rules = String.concat "\n" (List.map diff_rule variants) in
    let foreign = if simd then {|
 (foreign_archives simd_stubs)|} else "" in
    let subst = function
      | "enabled_if" -> mk_enabled_if plat
      | "enabled_if_with_lldb" -> mk_enabled_if_with_lldb plat
      | "enabled_if_without_lldb" -> mk_enabled_if_without_lldb plat
      | "name" -> name
      | "foreign" -> foreign
      | "diff_rules" -> diff_rules
      | "filter" -> "filter_for_function_call_only.sh"
      | _ -> assert false
    in
    Buffer.clear buf;
    Buffer.add_substitute buf subst
      {|
(executable
 (name ${name})
 (modules ${name})
 ${enabled_if}
 (libraries stdlib_stable)
 (ocamlopt_flags
  (:standard -g -gno-upstream-dwarf -bin-annot-cms -gdwarf-fidelity high
   -shape-format debugging-shapes -extension simd_beta -gdwarf-pedantic
   -name-mangling-scheme %{env:OXCAML_NAME_MANGLING=flat}
   (:include function_sections_flags.sexp)))${foreign})

(rule
 ${enabled_if_with_lldb}
 (targets ${name}.output.corrected)
 (deps ${name}.exe ${name}.lldb ${filter})
 (action
  (progn
   (bash
    "sed -e 's/^(lldb) //' -e '/^[[:space:]]*$/d' ${name}.lldb > \
     ${name}_clean.lldb")
   (with-outputs-to ${name}.output.corrected
    (pipe-outputs
     (run %{env:OXCAML_LLDB=} -s ${name}_clean.lldb ./${name}.exe)
     (run sh ./${filter}))))))

(rule
 ${enabled_if_without_lldb}
 (targets ${name}.output.corrected)
 (deps ${name}.exe)
 (action
  (progn
   (echo
    "ERROR: OXCAML_LLDB environment variable not set.\n\
DWARF tests require a custom LLDB build. Please set OXCAML_LLDB to \
the path of your custom LLDB binary.\n\
Example: export OXCAML_LLDB=/path/to/custom/lldb")
   (bash "exit 1"))))

${diff_rules}|};
    Buffer.output_buffer Out_channel.stdout buf
  in
  (* Generate tests - add more tests here as needed *)
  print_dwarf_test "test_basic_dwarf";
  print_dwarf_test "test_unboxed_dwarf";
  print_dwarf_test "test_datatypes_dwarf";
  print_dwarf_test ~simd:true "test_simd_dwarf";
  print_dwarf_test "test_simple_functor_dwarf";
  print_dwarf_test "test_parameters_dwarf";
  print_dwarf_test "test_callstack_dwarf";
  print_dwarf_test "test_stepping_dwarf";
  print_dwarf_test "test_closures_dwarf";
  print_dwarf_test "test_large_data_dwarf";
  print_dwarf_test "test_tailrec_dwarf";
  print_dwarf_test "test_ocaml_and_c_dwarf";
  ()
