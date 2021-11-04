(**
  * Constants and library functions for generating quantum gate matrices.
  *)

open Lacamlext;;
open Lacaml.Z;;

(* General Constants and Utils *)
let c0 = Complex.zero;;
let c1 = Complex.one;;
let ci = Complex.i;;

let r2o2 = Float.div 1.0 (sqrt 2.);;
let cr2o2 = Cenv.c r2o2 0.;;

let log2 x = Float.div (Float.log10 x) (Float.log10 2.);;

module States = struct

  let zero_state_vec = (
    Vec.of_array [|
      c1;
      c0;
    |]
  )

  let one_state_vec = (
    Vec.of_array [|
      c0;
      c1;
    |]
  )

  let plus_state_vec = (
    Vec.of_array [|
      cr2o2;
      cr2o2;
    |]
  )

  let minus_state_vec = (
    let open Cenv in
    Vec.of_array [|
      cr2o2;
      -cr2o2;
    |]
  )

  let i_state_vec = (
    let open Cenv in
    Vec.of_array [|
      cr2o2;
      cr2o2 * ci;
    |]
  )
  
  let negi_state_vec = (
    let open Cenv in
    Vec.of_array [|
      cr2o2;
      -(cr2o2 * ci);
    |]
  )

  let zero_state = Mat.from_col_vec zero_state_vec
  let one_state = Mat.from_col_vec one_state_vec
  let plus_state = Mat.from_col_vec plus_state_vec
  let minus_state = Mat.from_col_vec minus_state_vec
  let i_state = Mat.from_col_vec i_state_vec
  let negi_state = Mat.from_col_vec negi_state_vec

end;;


module Bases = struct
  
  let x_basis = (States.plus_state, States.minus_state)
  let y_basis = (States.i_state, States.negi_state)
  let z_basis = (States.zero_state, States.one_state)

end;;


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
    * Returns an operator that applies the unitary operator
    * `unitary` to all qubits in a system with `qubit_num` qubits.
    *)
  let to_all unitary qubit_num = (
    let rec helper temp_mat count = (
      if count > 1 then (
        let next_mat = Mat.tensor_prod temp_mat unitary in
        helper next_mat (count - 1)
      ) else temp_mat
    ) in
    helper unitary qubit_num
  )

  (**
    * Returns the unitary matrix required to change the base of 
    * a state vector from base A to B; i.e., B = UA.
    *)
  let change_base ?(qubits=1) (old1, old2) (new1, new2) = (
    let get_first m = List.hd (List.hd (Mat.to_list m)) in
    let e11 = get_first (gemm ~transa:`C old1 new1) in
    let e12 = get_first (gemm ~transa:`C old1 new2) in
    let e21 = get_first (gemm ~transa:`C old2 new1) in
    let e22 = get_first (gemm ~transa:`C old2 new2) in
    let unitary = Mat.of_array [|[| e11; e12 |]; [| e21; e22 |]|] in
    let unitary' = to_all unitary qubits in
    let iden = Mat.identity (Int.shift_left 1 qubits) in
    gemm ~transa:`C unitary' iden
  )

end;;


module DensityMatrix = struct

  let from_state_vector statevec = gemm ~transb:`C statevec statevec

  let purity densmat = (Mat.trace (gemm densmat densmat)).re

  let apply_operator op densmat = gemm ~transb:`C (gemm op densmat) op

  let change_base old_b new_b densmat qubit_num = (
    apply_operator (Gates.change_base ~qubits:qubit_num old_b new_b) densmat
  )

  let extract_info ?(print=false) densmat = (
    let as_array = Mat.to_array densmat in
    let len = Array.length as_array in
    let format_qubit i = (
      let bit_num = Float.to_int (log2 (Int.to_float len)) in 
      let rec helper bit = (
        if bit < bit_num then (
          let next = if i land (1 lsl bit) <> 0 then "1" else "0" in
          (helper (bit + 1)) ^ next
        ) else ""
      ) in
      helper 0
    ) in
    let info_tbl = Hashtbl.create len in
    let rec helper i = (
      if i < len then (
        let e = as_array.(i).(i) in (
          if print then (
            print_endline (format_qubit i ^ "\t" ^ Float.to_string e.re)
          );
          Hashtbl.add info_tbl i e.re;
          helper (i+1)
        )
      ) else ()
    ) in (
      if print then (
        print_endline "Qubits\tProbability"
      );
      helper 0;
      info_tbl
    )
  )

  module Measurement = struct

    let collapse ?(normalize=true) densmat op = (
      let result = apply_operator op densmat in
      if normalize then (
        (* Normalizes the result *)
        let trace = Mat.trace result in
        let one_over_trace = Cenv.(Complex.one / trace) in
        Mat.scal_mul one_over_trace result
      ) else result
    )

    let collapse_single ?(proj_down=true) ?(normalize=true) n q densmat op = (
      let op = if proj_down then (
        Mat.cleanup (gemm ~transa:`C op (Mat.identity 2))
      ) else op in
      let op' = Gates.gate op n q in
      collapse ~normalize:normalize densmat op'
    )

    let measure ?(proj_down=true) (base_a, base_b) n q densmat = (
      let base_a_op = if proj_down then base_a else from_state_vector base_a in
      let base_b_op = if proj_down then base_b else from_state_vector base_b in

      (* Creates the new measured density matrix *)
      let collapse_single' = collapse_single n q densmat ~normalize:false ~proj_down:proj_down in
      let base_a_measurement = collapse_single' base_a_op in
      let base_b_measurement = collapse_single' base_b_op in
      (base_a_measurement, base_b_measurement)
    )

  end

end;;


module StateVector = struct

  let to_density_matrix statevec = (
    DensityMatrix.from_state_vector statevec
  )

  let change_base old_b new_b statevec qubit_num = (
    let new_mat = gemm (Gates.change_base ~qubits:qubit_num old_b new_b) statevec in
    Mat.cleanup new_mat
  )

  let extract_info ?(print=false) statevec = (
    DensityMatrix.extract_info ~print:print (to_density_matrix statevec)
  )
    
  module Measurement = struct

    (**
      * Returns the projection of a state |x>; i.e., returns |x><x|.
      *)
    let project m = gemm ~transb:`C m m

    (**
      * Collapses a state vector `statevec` using the projector `proj`
      *)
    let collapse (statevec : Mat.t) (proj : Mat.t) = (
      let result = gemm proj statevec in
      (* Renormalizes the result. *)
      let mag = Mat.cleanup (gemm ~transa:`C result result) in
      let one_over_mag = Cenv.(Complex.one / (Mat.to_array mag).(0).(0)) in
      Mat.cleanup (Mat.scal_mul one_over_mag result)
    )

    (**
      * Collapses a single qubit `q` of an `n` qubit system represented
      * as a state vector `statevec` using the single qubit projector `proj`.
      *)
    let collapse_single ?(proj_down=true) n q (statevec : Mat.t) (proj : Mat.t) = (
      
      let proj = if proj_down then (
        Mat.cleanup (gemm ~transa:`C proj (Mat.identity 2))
      ) else proj in
      let proj' = Gates.gate proj n q in
      (*let _ = Mat.print proj' in*)
      collapse statevec proj'
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
    let prob_single ?(proj_down=true) n q (statevec : Mat.t) (proj : Mat.t) = (
      let proj = if proj_down then project proj else proj in
      let proj' = Gates.gate proj n q in
      prob statevec proj'
    )

    let measure ?(proj_down=true) (base_a, base_b) n q statevec = (
      (* Calculates the probabilities given the projectors and statevector *)
      let base_a_projector = if proj_down then base_a else project base_a in
      let base_a_probability = prob_single n q statevec base_a_projector  ~proj_down:proj_down in
      (* Using the Random module, determines which state to collapse to *)
      let rand_val = Random.float 1. in
      let outcome = ref 0 in
      let projector = (
        if rand_val <= base_a_probability then (
          outcome := 0; (* for keeping track of the outcome *)
          base_a_projector
        ) else (
          outcome := 1;
          if proj_down then (
            base_b
          ) else project base_b (* base_b_projector *)
        )
      ) in
      let projector = Mat.cleanup projector in
      let statevec' = collapse_single n q statevec projector ~proj_down:proj_down in
      (*let _ = Mat.print statevec' in*)
      (statevec', !outcome)
    )

  end

end;;
