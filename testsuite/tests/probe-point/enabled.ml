(* TEST
 modules = "enable_stubs.c";
 probes;
 arch_amd64;
 linux;
 {
   native;
 }{
   flags = "-O3";
   native;
 }{
   flags = "-Oclassic";
   native;
 }{
   flags = "-no-probes-optimized";
   native;
 }{
   flags = "-dcfg-invariants";
   native;
 }
*)

external enable_probe : string -> unit = "test_enable_probe"

let read counts = Array.to_list (Obj.magic_uncontended counts : int array)

let print name counts =
  Printf.printf "%s: [%s]\n" name
    (String.concat "; " (List.map Int.to_string (read counts)))

(* A leaf function, which may not have a frame. *)
let[@inline never] leaf x =
  [%probe_point "a" 0];
  if x > 0 then [%probe_point "a" 1] else [%probe_point "a" 2];
  x + 1

(* Many values are live across the probe points, so their registers must be
   preserved by the stub. *)
let[@inline never] live (a : int) (b : int) (c : int) (d : int) (e : float)
    (f : float) =
  let g = a * b and h = c * d and i = e *. f in
  [%probe_point "a" 3];
  let j = g + h + a + b + c + d in
  [%probe_point "a" 3];
  let k = i +. e +. f in
  [%probe_point "a" 3];
  j + int_of_float k

let evaluated = ref 0

let[@inline never] conditional x =
  [%probe_point "a" 4
     (incr evaluated;
      x mod 2 = 0)];
  [%probe_point "b" 0 (x > 2)]

let[@inline never] disabled () = [%probe_point "b" 1]

let run () =
  for i = 0 to 4 do
    ignore (Sys.opaque_identity (leaf (i - 2)))
  done;
  Printf.printf "live: %d\n" (live 1 2 3 4 1.5 2.5);
  for i = 0 to 5 do
    conditional i
  done;
  disabled ();
  print "a" [%probe_point_counts "a"];
  print "b" [%probe_point_counts "b"];
  Printf.printf "evaluated: %d\n" !evaluated

let () =
  run ();
  enable_probe "a";
  run ();
  run ();
  let counts () : int array = Obj.magic_uncontended [%probe_point_counts "a"] in
  Printf.printf "same array: %b\n" (counts () == counts ())
