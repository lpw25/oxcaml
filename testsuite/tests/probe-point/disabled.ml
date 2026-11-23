(* TEST
 {
   bytecode;
 }{
   native;
 }{
   flags = "-no-probes";
   native;
 }
*)

(* Probe points are disabled unless enabled by an external tool, so the
   counts stay at zero and conditions are not evaluated. *)

let read counts = Array.to_list (Obj.magic_uncontended counts : int array)

let print name counts =
  Printf.printf "%s: [%s]\n" name
    (String.concat "; " (List.map Int.to_string (read counts)))

let evaluated = ref 0

let[@inline never] f x =
  [%probe_point "a" 0];
  [%probe_point "a" 5
     (incr evaluated;
      x > 0)];
  [%probe_point "b" 2];
  x + 1

let () =
  (* The counts can be referred to before the points. *)
  print "a" [%probe_point_counts "a"];
  for i = 0 to 9 do
    ignore (Sys.opaque_identity (f i))
  done;
  print "a" [%probe_point_counts "a"];
  print "b" [%probe_point_counts "b"];
  Printf.printf "evaluated: %d\n" !evaluated
