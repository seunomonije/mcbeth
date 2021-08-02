(**
  * Constants and library functions for generating quantum gate matrices.
  *)

open Lacamlext;;
open Lacaml.Z;;

(* Commonly Used Constants *)
let c0 = Cenv.c 0. 0.;;
let c1 = Cenv.c 1. 0.;;
let ci = Cenv.c 0. 1.;;

let r2o2 = Float.div 1.0 (sqrt 2.);;


module States = struct

  let zero_state_vec = (
    let open Cenv in
    Vec.of_array [|
      c 1. 0.;
      c 0. 0.;
    |]
  )

  let one_state_vec = (
    let open Cenv in 
    Vec.of_array [|
      c 0. 0.;
      c 1. 0.;
    |]
  )

  let plus_state_vec = (
    let open Cenv in 
    Vec.of_array [|
      c r2o2 0.;
      c r2o2 0.;
    |]
  )

  let minus_state_vec = (
    let open Cenv in 
    Vec.of_array [|
      c r2o2 0.;
      c (-.r2o2) 0.;
    |]
  )

  let zero_state = Mat.from_col_vec zero_state_vec
  let one_state = Mat.from_col_vec one_state_vec
  let plus_state = Mat.from_col_vec plus_state_vec
  let minus_state = Mat.from_col_vec minus_state_vec

end

module Gates = struct

  (* Pauli Matrices *)
  let single_pauli_x = (
    Mat.of_array [|
      [| c0; c1 |];
      [| c1; c0 |];
    |]
  )

  let single_pauli_y = (
    let open Cenv in
    Mat.of_array [|
      [| c0; -ci |];
      [| ci; c0 |];
    |]
  )

  let single_pauli_z = (
    let open Cenv in
    Mat.of_array [|
      [| c1; c0 |];
      [| c0; -c1 |];
    |]
  )

  let iden x = Mat.identity (Int.shift_left 1 x)

  (**
    * Creates a gate matrix for an `n`-qubit system acting on qubit `q` with gate `u`.
    *)
  let gate u n q = (
    let ( * ) = Mat.tensor_prod in
    (iden q) * u * (iden (n - q - 1))
  )

  let pauli_x n q = gate single_pauli_x n q
  let pauli_y n q = gate single_pauli_y n q
  let pauli_z n q = gate single_pauli_z n q


  (**
    * Controlled-U matrix for an `n`-qubit system acting on two qubits, `q1` and `q2`;
    * `q1` is the control. `u` is the 2 by 2 matrix representing the desired gate.
    *
    * Ref: https://quantumcomputing.stackexchange.com/a/4255/16264
    *)
  let controlled u n q1 q2 = (
    let proj0 = Mat.of_array [|[| c1; c0 |]; [| c0; c0 |]|] in
    let proj1 = Mat.of_array [|[| c0; c0 |]; [| c0; c1 |]|] in
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
  )

  let ctrl_x n q1 q2 = controlled single_pauli_x n q1 q2
  let ctrl_y n q1 q2 = controlled single_pauli_y n q1 q2
  let ctrl_z n q1 q2 = controlled single_pauli_z n q1 q2

  let cnot = ctrl_x

  (**
    * Returns the unitary matrix required to change the base of 
    * a state vector from base A to B; i.e., B = UA.
    *)
  let change_base (old1, old2) (new1, new2) = (
    let get_first m = List.hd (List.hd (Mat.to_list m)) in
    let e11 = get_first (gemm ~transa:`C old1 new1) in
    let e12 = get_first (gemm ~transa:`C old1 new2) in
    let e21 = get_first (gemm ~transa:`C old2 new1) in
    let e22 = get_first (gemm ~transa:`C old2 new2) in
    let unitary = Mat.of_array [|[| e11; e12 |]; [| e21; e22 |]|] in
    let iden = Mat.identity 2 in
    gemm ~transa:`C unitary iden
  )

end;;


module DensityMatrix = struct

  let from_state_vector statevec = gemm ~transb:`C statevec statevec

  let purity densmat = (Mat.trace (gemm densmat densmat)).re

  let apply_operator op densmat = gemm ~transb:`C (gemm op densmat) op

  let change_base old_b new_b densmat = (
    apply_operator (Gates.change_base old_b new_b) densmat
  )

  module Measurement = struct

    let measure ?(normalize=true) densmat op = (
      let result = apply_operator op densmat in
      if normalize then (
        let trace = Mat.trace result in
        let one_over_trace = Cenv.((c 1. 0.) / trace) in
        Mat.scal_mul one_over_trace result
      ) else result
    )

    let measure_single ?(normalize=true) n q densmat op = (
      let op' = Gates.gate op n q in
      measure ~normalize:normalize densmat op'
    )

  end

end;;

module StateVector = struct

  let to_density_matrix statevec = (
    DensityMatrix.from_state_vector statevec
  )

  let purity statevec = (
    DensityMatrix.purity (DensityMatrix.from_state_vector statevec)
  )

  let change_base old_b new_b statevec = (
    gemm (Gates.change_base old_b new_b) statevec
  )
    
  module Measurement = struct

    (**
      * Returns the projection of a state |x>; i.e., returns |x><x|.
      *)
    let project m = gemm ~transb:`C m m

    (**
      * Collapses a state vector `statevec` using the projector `proj`
      *)
    let measure (statevec : Mat.t) (proj : Mat.t) = (
      let result = gemm proj statevec in
      let mag = Vec.mag (Mat.as_vec result) in
      let one_over_mag = Cenv.((c 1. 0.) / mag) in
      Mat.scal_mul one_over_mag result
    )

    (**
      * Collapses a single qubit `q` of an `n` qubit system represented
      * as a state vector `statevec` using the single qubit projector `proj`.
      *)
    let measure_single n q (statevec : Mat.t) (proj : Mat.t) = (
      let proj' = Gates.gate proj n q in
      measure statevec proj'
    )

    (**
      * Calculates the probability of collapsing `statevec` to the
      * state associated with the projector `proj`.
      *
      * Probability = <S| P |S>
      *
      *)
    let prob (statevec : Mat.t) (proj : Mat.t) = (
      let result_matrix = gemm (gemm ~transa:`C statevec proj) statevec in
      let result_complex = List.hd (List.hd (Mat.to_list result_matrix)) in
      result_complex.re (* Ignores the imaginary part, just returns the real *)
    )

    
    (**
      * Calculates the probability of collapsing a single qubit `q` of an 
      * `n` qubit system represented as a statevector `statevec` to the state
      * associated with the single qubit projector `proj`.
      *)
    let prob_single n q (statevec : Mat.t) (proj : Mat.t) = (
      let proj' = Gates.gate proj n q in
      prob statevec proj'
    )

  end

end;;