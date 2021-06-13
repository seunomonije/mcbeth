
open Lacamlext
open Lacaml.Z

val pauli_x : Mat.t
val pauli_y : Mat.t
val pauli_z : Mat.t

val controlled : Mat.t -> int -> int -> int -> Mat.t
val ctrl_x : int -> int -> int -> Mat.t
val ctrl_y : int -> int -> int -> Mat.t
val ctrl_z : int -> int -> int -> Mat.t

val cnot : int -> int -> int -> Mat.t
