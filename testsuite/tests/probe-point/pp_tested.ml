(* A parameterised library containing probe points. *)

let[@inline never] hit () =
  [%probe_point "pp" 0];
  Pp_counter.name

let counts () = [%probe_point_counts "pp"]
