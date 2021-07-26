
open Types

val print_prog : prog -> unit
val print_states : Lacaml.Z.Vec.t array -> unit

val calc_qubit_num : prog -> int

val well_formed : prog -> int

val calc_signal : (qubit -> int) -> qubit list -> int
val new_angle : (qubit -> int) -> float -> qubit list -> qubit list -> float

val prep_qubits : int -> prep list -> Lacaml.Z.Vec.t array
