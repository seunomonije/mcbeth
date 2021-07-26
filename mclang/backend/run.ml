(**
  * Implementation of Comamnds
  *)


open Types;;
open Utils;;

open Lacamlext;;
open Lacaml.Z;;

open Qlib.Gates;;



(**********************************************************************************
  *                                                                               *
  *                           Randomized Evaluation                               *
  *                                                                               *
  *********************************************************************************)

(**
  * Given a state vector, returns the outcome of a qubit `q`.
  * Assumes that the qubit has already been measured.
  *
  * Returns 0 if the state collapsed to |+_\alpha>.
  * Returns 1 if the state collapsed to |-_\alpha>.
  * Returns -1 if the qubit was never measured.
  *
  * \alpha is taken from a passed in hash table mapping the
  * qubit to its measurement angle.
  *)
let get_outcome mtbl q = (
  let _ = (mtbl, q) in 0
)


(**
  * Performs appropriate operations to execute command.
  *)
let eval_cmd qubit_num mtbl (statevec : Mat.t) (c : cmd) : Mat.t = (
  match c with 
  | Entangle (qubit1, qubit2) -> (
    (* Entagles qubit1 and qubit2 by performing a controlled-Z operation *)
    (* qubit1 is the control *)
    let _ = Mat.print statevec in
    gemm (ctrl_z qubit_num qubit1 qubit2) statevec
  )
  | Measure (qubit, angle, signals_s, signals_t) -> (
    let _ = Mat.print statevec in
    (* Calculates the angle of measurement *)
    let get_outcome' = get_outcome mtbl in
    let angle' = new_angle get_outcome' angle signals_s signals_t in
    let angle'' = Cenv.float_to_complex angle' in
    let _ = print_endline ("Angle: " ^ (Cenv.cstr angle'')) in
    (* Calculates the projectors *)
    let exp_const = Complex.exp (Cenv.(Complex.i * angle'')) in
    let r202 = Cenv.float_to_complex (Float.div 1.0 (sqrt 2.)) in
    let zero_state = Mat.from_col_vec Qlib.States.zero_state in
    let one_state = Mat.from_col_vec Qlib.States.one_state in
    let one_state_e = Mat.scal_mul exp_const one_state in
    let plus_state = Mat.scal_mul r202 (Mat.add zero_state one_state_e) in
    let minus_state = Mat.scal_mul r202 (Mat.sub zero_state one_state_e) in
    let plus_projector = Qlib.Measurement.project plus_state in
    let minus_projector = Qlib.Measurement.project minus_state in
    let _ = Mat.print plus_projector in
    let _ = Mat.print minus_projector in
    (* Calculates the probabilities given the projectors and statevector *)
    let plus_probability = Qlib.Measurement.prob_single qubit_num qubit statevec plus_projector in
    let minus_probability = Qlib.Measurement.prob_single qubit_num qubit statevec minus_projector in
    let _ = print_endline (Float.to_string plus_probability) in
    let _ = print_endline (Float.to_string minus_probability) in
    (* Using the Random module, determines which state to collapse to *)
    let rand_val = Random.float 1. in
    let _ = print_endline (Float.to_string rand_val) in
    let projector = if rand_val <= plus_probability then plus_projector else minus_projector in
    let statevec' = Qlib.Measurement.collapse_single qubit_num qubit statevec projector in
    let _ = Mat.print statevec' in
    statevec'
  )
  | XCorrect (qubit, signals) -> (
    let get_outcome' = get_outcome mtbl in
    (* Perform correction if not dependent (`signals == []`) or the signal is 1 *)
    if (List.length signals == 0) || (calc_signal get_outcome' signals > 0) then (
      gemm (pauli_x qubit_num qubit) statevec
    ) else statevec
  )
  | ZCorrect (qubit, signals) -> (
    let get_outcome' = get_outcome mtbl in
    (* Perform correction if not dependent (`signals == []`) or the signal is 1 *)
    if (List.length signals == 0) || (calc_signal get_outcome' signals > 0) then (
      gemm (pauli_z qubit_num qubit) statevec
    ) else statevec
  )
);;

let eval_cmds qubit_num statevec cmds = (
  let eval_cmd' = eval_cmd qubit_num (Hashtbl.create qubit_num) in
  List.fold_left (fun svec p -> eval_cmd' svec p) statevec cmds
);;


(**
  * Runs a program, evaluating the quantum measurements using random functions.
  * First eval checks if the function is well formed. It then it runs the program
  * and returns the result extracted from the state matrix.
  *)
let rand_eval ((preps, cmds) as p : prog) : bool list = (
  Random.self_init();
  let qubit_num = well_formed(p) in (
    if qubit_num > 0 then (
      let qubit_inits = prep_qubits qubit_num preps in
      let init_statevec = Mat.from_col_vec (Vec.tensor_prod_arr qubit_inits) in
      let statevec = Mat.as_vec (eval_cmds qubit_num init_statevec cmds) in (
        let _ = statevec in 
        [] (* TODO: Will pull bool list from evaluating matrix/vector stored in memory *)
      )
    ) else []
  )
);;


