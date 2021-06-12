
open Types

val print_prog : prog -> unit
val print_states : Lacaml.Z.Vec.t array -> unit
val well_formed : prog -> int
val prep_qubits : int -> prep list -> Lacaml.Z.Vec.t array

val foobar : unit -> unit
