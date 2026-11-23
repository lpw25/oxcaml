(* TEST
 expect;
*)

module M = struct
  let f x =
    [%probe_point "p" 0];
    [%probe_point "p" 7 (x > 0)]

  (* The counts are contended and portable. *)
  let (g @ portable) () = [%probe_point_counts "p"]

  let length () = Array.length [%probe_point_counts "p"]
end
[%%expect{|
module M :
  sig
    val f : int -> unit
    val g : unit -> int array @ contended
    val length : unit -> int
  end
|}]

let f () =
  [%probe_point "p" 0];
  Array.get [%probe_point_counts "p"] 0
[%%expect{|
Line 3, characters 12-37:
3 |   Array.get [%probe_point_counts "p"] 0
                ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value is "contended" but is expected to be "uncontended".
|}]

(* The counts may be referred to before the points. *)
let f () =
  let counts = [%probe_point_counts "p"] in
  [%probe_point "p" 0];
  Array.length counts
[%%expect{|
val f : unit -> int = <fun>
|}]

(* Conditions may refer to local values. *)
let f (x @ local) = [%probe_point "q" 0 (fst x > 0)]
[%%expect{|
val f : int * 'a @ local -> unit = <fun>
|}]

let f () = [%probe_point "r" 0 1]
[%%expect{|
Line 1, characters 31-32:
1 | let f () = [%probe_point "r" 0 1]
                                   ^
Error: The constant "1" has type "int" but an expression was expected of type
         "bool"
|}]

let f () = [%probe_point "r" (-1)]
[%%expect{|
Line 1, characters 29-33:
1 | let f () = [%probe_point "r" (-1)]
                                 ^^^^
Error: The index -1 of this %probe_point is out of range. Indices must be at least 0 and less than 1048576.
|}]

let f () = [%probe_point "r" 1048576]
[%%expect{|
Line 1, characters 29-36:
1 | let f () = [%probe_point "r" 1048576]
                                 ^^^^^^^
Error: The index 1048576 of this %probe_point is out of range. Indices must be at least 0 and less than 1048576.
|}]

let f () = [%probe_point "r" 1048575]
[%%expect{|
val f : unit -> unit = <fun>
|}]

let f () = [%probe_point "r"]
[%%expect{|
Line 1, characters 11-29:
1 | let f () = [%probe_point "r"]
               ^^^^^^^^^^^^^^^^^^
Error: %probe_point must consist of a name, as a string literal, followed by an index, as an integer literal, optionally followed by a single expression of type bool
|}]

let f i = [%probe_point "r" i]
[%%expect{|
Line 1, characters 10-30:
1 | let f i = [%probe_point "r" i]
              ^^^^^^^^^^^^^^^^^^^^
Error: %probe_point must consist of a name, as a string literal, followed by an index, as an integer literal, optionally followed by a single expression of type bool
|}]

let f () = [%probe_point "r" 0 true false]
[%%expect{|
Line 1, characters 11-42:
1 | let f () = [%probe_point "r" 0 true false]
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: %probe_point must consist of a name, as a string literal, followed by an index, as an integer literal, optionally followed by a single expression of type bool
|}]

let f () = [%probe_point "r" 0 ~cond:true]
[%%expect{|
Line 1, characters 11-42:
1 | let f () = [%probe_point "r" 0 ~cond:true]
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: %probe_point must consist of a name, as a string literal, followed by an index, as an integer literal, optionally followed by a single expression of type bool
|}]

let f () = [%probe_point_counts "undefined"]
[%%expect{|
Line 1, characters 32-43:
1 | let f () = [%probe_point_counts "undefined"]
                                    ^^^^^^^^^^^
Error: Undefined probe point name `undefined' used in %probe_point_counts. Not found [%probe_point "undefined" ...] in the same compilation unit.
|}]

let f () = [%probe_point_counts 0]
[%%expect{|
Line 1, characters 11-34:
1 | let f () = [%probe_point_counts 0]
               ^^^^^^^^^^^^^^^^^^^^^^^
Error: %probe_point_counts must specify a single probe name as a string literal
|}]
