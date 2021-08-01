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
  *                              Utility Functions                                *
  *                                                                               *
  *********************************************************************************)


(**
  * Inserts a new qubit with values of type `input` into a statevector 
  * `statevec` of size `n` at position `pos`. The position of the current 
  * qubit in the statevector at position `pos` is assumed to have value [[1] [1]].
  *)
let insert_qubit matrix n input pos = (
  let open Cenv in
  let zero = c 0. 0. in
  let one = c 1. 0. in
  let r2o2 = Float.div 1.0 (sqrt 2.) in
  let a, b = (
    match input with
    | Zero          -> (one, zero)
    | One           -> (zero, one)
    | Plus          -> (c r2o2 0., c r2o2 0.)
    | Minus         -> (c r2o2 0., c (-.r2o2) 0.)
    | State(c0, c1) -> (c0, c1)
  ) in
  let operator = Qlib.Gates.gate (Mat.of_array [|[| a; zero |]; [| zero; b |]|]) n pos in
  gemm operator matrix
);;

let handle_prep qubit_num matrix prep = (
  match prep with
  | Prep (qubit) -> (
    insert_qubit matrix qubit_num Plus qubit
  )
  | Input (qubit, input) -> (
    insert_qubit matrix qubit_num input qubit
  )
  | PrepList (qubits) -> (
    List.fold_left (fun m q -> insert_qubit m qubit_num Plus q) matrix qubits
  )
  | InputList (args) -> (
    List.fold_left (fun m (q, i) -> insert_qubit m qubit_num i q) matrix args
  )
  | _ -> matrix
);;

(**
  * Returns the outcome of a qubit `q`.
  *
  * `mtbl` maps qubits to 1 or 0 depending to what they were measured to.
  *
  * Returns 0 if the state collapsed to |+_\alpha>.
  * Returns 1 if the state collapsed to |-_\alpha>.
  * Returns -1 if the qubit was never measured.
  *)
let get_outcome mtbl q = (
  match Hashtbl.find_opt mtbl q with
  | Some(x) -> x
  | None    -> -1
);;


(**
  * Calculates signals -- i.e., a single number based on the outcomes of qubits.
  *
  * Signal s = \Sum_{i \in I}(s_i) where s_i = 0 if the measurement of qubit i
  * collapses the state to |+_\alpha> and s_i = 1 if the state collapses to
  * |-_\alpha>. The summation is performed in Z_2. 
  *
  * Input:
  *   get_outcome : a function which given a qubit, returns the outcome of the qubit,
  *                 should be 0 or 1; returns -1 on error.
  *   qs          : a list of qubits which the signal depends on
  * Output:
  *   Returns the signal, 0 or 1.
  *)
let calc_signal mtbl (qs : qubit list) = (
  (List.fold_left (fun s q -> s + get_outcome mtbl q) 0 qs) mod 2
);;


(**
  * Calculates the new angle of a measurement based on the original angle
  * and the outcomes of signals1 and signals2
  *
  * Input:
  *   get_outcome : a function to be passed to calc_signal; see calc_signal above
  *   angle       : the original angle \alpha of type float
  *   signals_s   : a list of qubits
  *   signals_t   : a list of qubits
  *
  * Output:
  *   Returns the new angle, of type float.
  *)
let new_angle mtbl angle signals_s signals_t = (
  let signal qs = float_of_int (calc_signal mtbl qs) in
  let ( + ) = Float.add in
  let ( * ) = Float.mul in
  (-1.)**(signal signals_s) * angle + (signal signals_t) * Float.pi
);;


let calc_measurement_states mtbl angle signals_s signals_t = (
  (* Calculates the angle of measurement *)
  let angle' = new_angle mtbl angle signals_s signals_t in
  let angle'' = Cenv.float_to_complex angle' in

  (* Calculates the projectors *)
  let exp_const = Complex.exp (Cenv.(Complex.i * angle'')) in
  let r202 = Cenv.float_to_complex (Float.div 1.0 (sqrt 2.)) in
  let zero_state = Qlib.States.zero_state_mat in
  let one_state = Qlib.States.one_state_mat in
  let one_state_e = Mat.scal_mul exp_const one_state in
  let plus_state = Mat.scal_mul r202 (Mat.add zero_state one_state_e) in
  let minus_state = Mat.scal_mul r202 (Mat.sub zero_state one_state_e) in
  (plus_state, minus_state)
)

let calc_correction mtbl signals application_func op matrix = (
  (* Perform correction if not dependent (`signals == []`) or the signal is 1 *)
  if (List.length signals == 0) || (calc_signal mtbl signals > 0) then (
    application_func op matrix
  ) else matrix
);;


(**********************************************************************************
  *                                                                               *
  *                           Randomized Evaluation                               *
  *                                                                               *
  *********************************************************************************)

(**
  * Performs appropriate operations to execute command.
  *)
let rand_eval_cmd_exec qubit_num mtbl (statevec : Mat.t) (c : cmd) : Mat.t = (
  match c with 
  | Entangle (qubit1, qubit2) -> (
    (* Entagles qubit1 and qubit2 by performing a controlled-Z operation *)
    (* qubit1 is the control *)
    gemm (ctrl_z qubit_num qubit1 qubit2) statevec
  )
  | Measure (qubit, angle, signals_s, signals_t) -> (
    let open Qlib.StateVector.Measurement in

    (* Calculates the projectors *)
    let (plus_state, minus_state) = calc_measurement_states mtbl angle signals_s signals_t in
    let plus_projector = project plus_state in
    let minus_projector = project minus_state in

    (* Calculates the probabilities given the projectors and statevector *)
    let plus_probability = prob_single qubit_num qubit statevec plus_projector in
    
    (* Using the Random module, determines which state to collapse to *)
    let rand_val = Random.float 1. in
    let projector = (
      if rand_val <= plus_probability then (
        Hashtbl.add mtbl qubit 0; (* keeps track of the outcome *)
        plus_projector
      ) else (
        Hashtbl.add mtbl qubit 1;
        minus_projector
      )
    ) in
    let statevec' = measure_single qubit_num qubit statevec projector in
    statevec'
  )
  | XCorrect (qubit, signals) -> (
    let gemm' a b = gemm a b in
    calc_correction mtbl signals gemm' (pauli_x qubit_num qubit) statevec
  )
  | ZCorrect (qubit, signals) -> (
    let gemm' a b = gemm a b in
    calc_correction mtbl signals gemm' (pauli_z qubit_num qubit) statevec
  )
  | prep -> handle_prep qubit_num statevec prep
);;


(**
  * Runs a program, evaluating the quantum measurements using random functions.
  * First eval checks if the function is well formed. It then it runs the program
  * and returns the resulting state vector.
  *)
let rand_eval (cmds : prog) : Vec.t = (
  Random.self_init();
  if well_formed cmds then (
    let qubit_num = calc_qubit_num cmds in
    let init_statevec = Mat.make (Int.shift_left 1 qubit_num) 1 (Cenv.c 1. 0.) in
    let eval_cmd' = rand_eval_cmd_exec qubit_num (Hashtbl.create qubit_num) in
    let statevec_mat = List.fold_left (fun sv p -> eval_cmd' sv p) init_statevec cmds in
    Mat.as_vec statevec_mat
  ) else Vec.empty
);;



(**********************************************************************************
  *                                                                               *
  *                                   Simulate                                    *
  *                                                                               *
  *********************************************************************************)

(**
  * Performs appropriate operations to execute command.
  *)
let simulate_cmd_exec qubit_num mtbl (densmat : Mat.t) (c : cmd) : Mat.t = (
  let open Qlib.DensityMatrix in
  match c with 
  | Entangle (qubit1, qubit2) -> (
    (* Entagles qubit1 and qubit2 by performing a controlled-Z operation *)
    (* qubit1 is the control *)
    apply_operator (ctrl_z qubit_num qubit1 qubit2) densmat
  )
  | Measure (qubit, angle, signals_s, signals_t) -> (
    let open Qlib.DensityMatrix.Measurement in

    (* Calculates the measurement operators *)
    let (plus_state, minus_state) = calc_measurement_states mtbl angle signals_s signals_t in
    let plus_op = from_state_vector plus_state in
    let minus_op = from_state_vector minus_state in

    (* Creates the new measured density matrix *)
    let plus_measurement = measure_single qubit_num qubit densmat plus_op ~normalize:false in
    let minus_measurement = measure_single qubit_num qubit densmat minus_op ~normalize:false in
    Mat.add plus_measurement minus_measurement
  )
  | XCorrect (qubit, signals) -> (
    calc_correction mtbl signals apply_operator (pauli_x qubit_num qubit) densmat
  )
  | ZCorrect (qubit, signals) -> (
    calc_correction mtbl signals apply_operator (pauli_z qubit_num qubit) densmat
  )
  | prep -> handle_prep qubit_num densmat prep
);;


(**
  * Runs a program, evaluating the operations using density matrices.
  * First eval checks if the function is well formed. It then it runs the program
  * and returns the resulting state vector.
  *)
let simulate (cmds : prog) : Mat.t = (
  if well_formed cmds then (
    let qubit_num = calc_qubit_num cmds in
    let size = Int.shift_left 1 qubit_num in
    let init_densmat = Mat.make size size (Cenv.c 1. 0.) in
    let eval_cmd' = simulate_cmd_exec qubit_num (Hashtbl.create qubit_num) in
    List.fold_left (fun dm p -> eval_cmd' dm p) init_densmat cmds
  ) else Mat.empty
);;


