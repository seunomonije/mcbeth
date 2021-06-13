(**
  * Constants and library functions for generating quantum gate matrices.
  *)

open Lacamlext;;
open Lacaml.Z;;


(* Commonly Used Complex Constants *)
let c0 = Cenv.c 0. 0.;;
let c1 = Cenv.c 1. 0.;;
let ci = Cenv.c 0. 1.;;


(* Pauli Matrices *)
let pauli_x = (
  Mat.of_array [|
    [| c0; c1 |];
    [| c1; c0 |];
  |]
);;

let pauli_y = (
  let open Cenv in
  Mat.of_array [|
    [| c0; -ci |];
    [| ci; c0 |];
  |]
);;

let pauli_z = (
  let open Cenv in
  Mat.of_array [|
    [| c1; c0 |];
    [| c0; -c1 |];
  |]
);;


(**
  * Controlled-U matrix for an `n`-qubit system acting on two qubits, `q1` and `q2`;
  * `q1` is the control. `u` is the 2 by 2 matrix representing the desired gate.
  *
  * Ref: https://quantumcomputing.stackexchange.com/a/4255/16264
  *)
let controlled u n q1 q2 = (
  let proj0 = Mat.of_array [|[| c1; c0 |]; [| c0; c0 |]|] in
  let proj1 = Mat.of_array [|[| c0; c0 |]; [| c0; c1 |]|] in
  let iden x = Mat.identity (Int.shift_left 1 x) in
  let ( * ) = Mat.tensor_prod in
  let left = (iden q1) * proj0 * (iden (n - q1 - 1)) in
  let right = (
    if q1 < q2 then (
      let x = q1 in
      let z = n - q2 - 1 in
      let y = q2 - q1 - 1 in
      (iden x) * proj1 * (iden y) * u * (iden z)
    ) else if q2 < q1 then (
      let x = q2 in
      let z = n - q1 - 1 in
      let y = q1 - q2 - 1 in
      (iden x) * u * (iden y) * proj1 * (iden z)
    ) else (
      (* TODO: raise error *)
      Mat.empty
    )
  ) in
  Mat.add left right
);;

let ctrl_x n q1 q2 = controlled pauli_x n q1 q2;;
let ctrl_y n q1 q2 = controlled pauli_y n q1 q2;;
let ctrl_z n q1 q2 = controlled pauli_z n q1 q2;;

let cnot = ctrl_x;;