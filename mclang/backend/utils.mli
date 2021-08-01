
open Types

val print_prog : prog -> unit
val print_states : Lacaml.Z.Vec.t array -> unit

val calc_qubit_num : prog -> int

val well_formed : prog -> bool

(* val prep_qubits : int -> prep list -> Lacaml.Z.Vec.t array *)
