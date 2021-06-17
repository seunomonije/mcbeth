
open Lacamlext
open Lacaml.Z

val single_pauli_x : Mat.t
val single_pauli_y : Mat.t
val single_pauli_z : Mat.t

val gate : Mat.t -> int -> int -> Mat.t
val pauli_x : int -> int -> Mat.t
val pauli_y : int -> int -> Mat.t
val pauli_z : int -> int -> Mat.t

val controlled : Mat.t -> int -> int -> int -> Mat.t
val ctrl_x : int -> int -> int -> Mat.t
val ctrl_y : int -> int -> int -> Mat.t
val ctrl_z : int -> int -> int -> Mat.t

val cnot : int -> int -> int -> Mat.t
