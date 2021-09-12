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
let insert_qubit ?(densmat=false) matrix n input pos = (
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
  let temp = gemm operator matrix in
  if densmat then (
    gemm ~transb:`C temp operator
  ) else temp
);;

let handle_prep ?(densmat=false) qubit_num matrix prep = (
  let insert_qubit = insert_qubit ~densmat:densmat in
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
  let zero_state = Qlib.States.zero_state in
  let one_state = Qlib.States.one_state in
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
  *                     Randomized Evaluation (Weak Simulation)                   *
  *                                                                               *
  *********************************************************************************)

(**
  * Performs appropriate operations to execute command.
  *)
let rand_eval_cmd_exec qubit_num mtbl statevec c = (
  match c with 
  | Entangle (qubit1, qubit2) -> (
    (* Entagles qubit1 and qubit2 by performing a controlled-Z operation *)
    (* qubit1 is the control *)
    gemm (ctrl_z qubit_num qubit1 qubit2) statevec
  )
  | Measure (qubit, angle, signals_s, signals_t) -> (
    let open Qlib.StateVector.Measurement in

    (* Calculates the bases *)
    let bases = calc_measurement_states mtbl angle signals_s signals_t in

    let (statevec', outcome) = measure bases qubit_num qubit statevec in (
      Hashtbl.add mtbl qubit outcome; (* keeps track of the outcome *)
      statevec'
    )
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
let rand_eval ?(shots=0) (cmds : prog) : Mat.t = (
  Random.self_init();
  if well_formed cmds then (
    let qubit_num = calc_qubit_num cmds in
    let vec_size = Int.shift_left 1 qubit_num in
    let run_once = (
      let init_statevec = Mat.make vec_size 1 Complex.one in
      let exec_cmd' = rand_eval_cmd_exec qubit_num (Hashtbl.create qubit_num) in
      let statevec_mat = List.fold_left (fun sv p -> exec_cmd' sv p) init_statevec cmds in
      Mat.cleanup statevec_mat
    ) in
    if shots == 0 then (
      (* Run weak simulation once; don't perform a read-out measurements. *)
      run_once
    ) else (
      (* Run weak simulation `shots` times, performing a read-out measurement each time and averaging the results. *)
      let rec helper n sum = (
        if n > 0 then (
          (* Runs and applies read-out measurements to output qubits. *)
          let out_qubits = get_output_qubits cmds in
          let measure = Qlib.StateVector.Measurement.measure (Qlib.Bases.z_bases) qubit_num in
          let res = Hashtbl.fold (fun q _ vec -> (let (r, _) = measure q vec in r)) out_qubits (run_once) in
          helper (n-1) (Mat.add res sum)
        ) else sum
      ) in
      let total = helper shots (Mat.make vec_size 1 Complex.zero) in
      Mat.scal_mul (Cenv.(Complex.one / (c (Int.to_float shots) 0.))) total
    )
  ) else Mat.empty
);;



(**********************************************************************************
  *                                                                               *
  *                         Simulate (Strong Simulation)                          *
  *                                                                               *
  *********************************************************************************)

(**
  * Performs appropriate operations to execute command.
  *)
let simulate_cmd_exec qubit_num mtbl densmat c = (
  let open Qlib.DensityMatrix in
  match c with 
  | Entangle (qubit1, qubit2) -> (
    (* Entagles qubit1 and qubit2 by performing a controlled-Z operation *)
    (* qubit1 is the control *)
    apply_operator (ctrl_z qubit_num qubit1 qubit2) densmat
  )
  | Measure (qubit, angle, signals_s, signals_t) -> (
    let open Qlib.DensityMatrix.Measurement in

    (* Calculates the measurement bases *)
    let bases = calc_measurement_states mtbl angle signals_s signals_t in
    measure bases qubit_num qubit densmat

    (* TODO: Need to handle the outcom of dependent commands! (How?) *)
  )
  | XCorrect (qubit, signals) -> (
    calc_correction mtbl signals apply_operator (pauli_x qubit_num qubit) densmat
  )
  | ZCorrect (qubit, signals) -> (
    calc_correction mtbl signals apply_operator (pauli_z qubit_num qubit) densmat
  )
  | prep -> handle_prep ~densmat:true qubit_num densmat prep
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
    let exec_cmd' = simulate_cmd_exec qubit_num (Hashtbl.create qubit_num) in
    List.fold_left (fun dm p -> exec_cmd' dm p) init_densmat cmds
  ) else Mat.empty
);;


