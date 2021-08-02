
open Types

val print_prog : prog -> unit

val calc_qubit_num : prog -> int

val well_formed : prog -> bool

val parse_pattern : pattern -> prog

(* val prep_qubits : int -> prep list -> Lacaml.Z.Vec.t array *)
