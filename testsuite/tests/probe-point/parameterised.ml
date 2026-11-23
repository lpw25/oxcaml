(* TEST
 readonly_files = "\
   enable_stubs.c \
   pp_counter.mli \
   pp_arg_a.ml pp_arg_a.mli \
   pp_arg_b.ml pp_arg_b.mli \
   pp_tested.ml pp_tested.mli \
 ";
 probes;
 arch_amd64;
 linux;

 setup-ocamlopt.byte-build-env;

 flags = "-as-parameter";
 module = "pp_counter.mli";
 ocamlopt.byte;

 flags = "-as-argument-for Pp_counter";
 module = "pp_arg_a.mli pp_arg_a.ml pp_arg_b.mli pp_arg_b.ml";
 ocamlopt.byte;

 flags = "-parameter Pp_counter";
 module = "pp_tested.mli pp_tested.ml";
 ocamlopt.byte;

 flags = "-instantiate";
 module = "";
 program = "pp_tested-Pp_arg_a.cmx";
 all_modules = "pp_tested.cmx pp_arg_a.cmx";
 ocamlopt.byte;

 flags = "-instantiate";
 module = "";
 program = "pp_tested-Pp_arg_b.cmx";
 all_modules = "pp_tested.cmx pp_arg_b.cmx";
 ocamlopt.byte;

 flags = "-w -misplaced-attribute";
 module = "parameterised.ml";
 program = "";
 all_modules = "";
 ocamlopt.byte;

 flags = "";
 module = "";
 program = "parameterised.exe";
 all_modules = "\
   enable_stubs.c \
   pp_arg_a.cmx \
   pp_arg_b.cmx \
   pp_tested.cmx \
   pp_tested-Pp_arg_a.cmx \
   pp_tested-Pp_arg_b.cmx \
   parameterised.cmx \
 ";
 ocamlopt.byte;

 program = "${test_build_directory}/parameterised.exe";
 run;

 check-program-output;
*)

(* The probe points of a parameterised library are shared by all of its
   instances: the counts cover the library's source code, whichever instances
   run it. *)

external enable_probe : string -> unit = "test_enable_probe"

module A = Pp_tested (Pp_counter) (Pp_arg_a) [@jane.non_erasable.instances]

module B = Pp_tested (Pp_counter) (Pp_arg_b) [@jane.non_erasable.instances]

let read counts = Array.to_list (Obj.magic_uncontended counts : int array)

let print () =
  Printf.printf "a: [%s] b: [%s] same array: %b\n"
    (String.concat "; " (List.map Int.to_string (read (A.counts ()))))
    (String.concat "; " (List.map Int.to_string (read (B.counts ()))))
    (Obj.magic_uncontended (A.counts ()) == Obj.magic_uncontended (B.counts ()))

let () =
  print_endline (A.hit () ^ B.hit ());
  print ();
  enable_probe "pp";
  print_endline (A.hit () ^ B.hit () ^ B.hit ());
  print ()
