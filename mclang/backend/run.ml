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
let insert_qubit ?(densmat=false) matrix input = (
  let open Cenv in
  let zero = Complex.zero in
  let one = Complex.one in
  let r2o2 = Float.div 1.0 (sqrt 2.) in
  let a, b = (
    match input with
    | Zero          -> (one, zero)
    | One           -> (zero, one)
    | Plus          -> (c r2o2 0., c r2o2 0.)
    | Minus         -> (c r2o2 0., c (-.r2o2) 0.)
    | State(c0, c1) -> (c0, c1)
  ) in
  let entry = Mat.of_array [|[| a |]; [| b |]|] in
  let entry' = if densmat then Qlib.DensityMatrix.from_state_vector entry else entry in
  let matrix' = Mat.tensor_prod matrix entry' in
  matrix'
);;

let handle_prep ?(densmat=false) matrix prep = (
  let insert_qubit = insert_qubit ~densmat:densmat in
  match prep with
  | Prep (_) -> (
    insert_qubit matrix Plus 
  )
  | Input (_, input) -> (
    insert_qubit matrix input
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
let calc_signal ?(mtbl_lock=None) mtbl (qs : qubit list) = (
  let exec_cmd () = (List.fold_left (fun s q -> s + get_outcome mtbl q) 0 qs) mod 2 in
  match mtbl_lock with
  | None -> exec_cmd ()
  | Some(lock) -> (
    Mutex.lock lock;
    let res = exec_cmd () in (
      Mutex.unlock lock;
      res
    )
  )
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
let new_angle ?(mtbl_lock=None) mtbl angle signals_s signals_t = (
  let signal qs = float_of_int (calc_signal mtbl qs ~mtbl_lock:mtbl_lock) in
  let ( + ) = Float.add in
  let ( * ) = Float.mul in
  (-1.)**(signal signals_s) * angle + (signal signals_t) * Float.pi
);;


let calc_measurement_states ?(mtbl_lock=None) mtbl angle signals_s signals_t = (
  (* Calculates the angle of measurement *)
  let angle' = new_angle mtbl angle signals_s signals_t ~mtbl_lock:mtbl_lock in

  (* Calculates the projectors *)
  Qlib.Bases.from_angle angle'
);;

let update_qtbl qtbl to_remove = (
  let update_list = ref [] in (
    Hashtbl.iter (fun q pos -> (
      if (q > to_remove) then (
        update_list := (q, pos)::(!update_list)
      )
    )) qtbl;
    List.iter (fun (q, pos) -> Hashtbl.replace qtbl q (pos-1)) (!update_list)
  );
  Hashtbl.remove qtbl to_remove
);;

let get_readout mtbl = (
  let readout = Hashtbl.create 0 in (
    Hashtbl.iter (fun q o -> (
      if q < 0 then (
        Hashtbl.add readout (-q - 1) o
      )
    )) mtbl;
    readout
  )
);;


(**********************************************************************************
  *                                                                               *
  *                     Randomized Evaluation (Weak Simulation)                   *
  *                                                                               *
  *********************************************************************************)

(**
  * Performs appropriate operations to execute command.
  *)
let rand_eval_cmd_exec ?(mtbl_lock=None) mtbl qtbl statevec c = (
  let result = (
    let getPos x = Hashtbl.find qtbl x in
    let qubit_num = (Hashtbl.length qtbl) in
    match c with 
    | Entangle (qubit1, qubit2) -> (
      (* Entagles qubit1 and qubit2 by performing a controlled-Z operation *)
      (* qubit1 is the control *)
      gemm (ctrl_z qubit_num (getPos qubit1) (getPos qubit2)) statevec
    )
    | Measure (qubit, angle, signals_s, signals_t) -> (
      let open Qlib.StateVector.Measurement in

      (* Calculates the bases *)
      let bases = calc_measurement_states mtbl angle signals_s signals_t ~mtbl_lock:mtbl_lock in

      (*let _ = Mat.print statevec in*)
      let (statevec', outcome) = measure bases qubit_num (getPos qubit) statevec in (
        (*print_endline (Int.to_string outcome);*)
        update_qtbl qtbl qubit; (* updates qubit positions in vector mapping *)
        
        (match mtbl_lock with
        | None -> ()
        | Some(lock) -> Mutex.lock lock);
        
        Hashtbl.add mtbl qubit outcome; (* keeps track of the outcome *)

        (match mtbl_lock with
        | None -> ()
        | Some(lock) -> Mutex.unlock lock);
        
        statevec'
      )
    )
    | XCorrect (qubit, signals) -> (
      (* Perform correction if not dependent (`signals == []`) or the signal is 1 *)
      if (List.length signals == 0) || (calc_signal mtbl signals ~mtbl_lock:mtbl_lock > 0) then (
        gemm (pauli_x qubit_num (getPos qubit)) statevec
      ) else statevec
    )
    | ZCorrect (qubit, signals) -> (
      (* Perform correction if not dependent (`signals == []`) or the signal is 1 *)
      if (List.length signals == 0) || (calc_signal mtbl signals ~mtbl_lock:mtbl_lock > 0) then (
        gemm (pauli_z qubit_num (getPos qubit)) statevec
      ) else statevec
    )
    | ReadOut (qubit, basis) -> (
      let open Qlib.StateVector.Measurement in

      (* Calculates the bases *)
      let bases = (
        match basis with
        | X -> Qlib.Bases.x_basis
        | Y -> Qlib.Bases.y_basis
        | Z -> Qlib.Bases.z_basis
        | Comp -> Qlib.Bases.z_basis
        | FromTuples(a, b) -> Qlib.Bases.from_tuples a b
        | FromAngle(a) -> Qlib.Bases.from_angle a
      ) in
      let _ = (
        let (a, b) = bases in (
          Mat.print a;
          Mat.print b;
          Mat.print statevec
        )
      ) in

      (*let _ = Mat.print statevec in*)
      let (statevec', outcome) = measure bases qubit_num (getPos qubit) statevec in (
        (*print_endline (Int.to_string outcome);*)
        update_qtbl qtbl qubit; (* updates qubit positions in vector mapping *)
        
        (match mtbl_lock with
        | None -> ()
        | Some(lock) -> Mutex.lock lock);
        
        Hashtbl.add mtbl (-qubit - 1) outcome; (* keeps track of the outcome *)

        (match mtbl_lock with
        | None -> ()
        | Some(lock) -> Mutex.unlock lock);

        Mat.print statevec'; 
        statevec'
      )
    )
    | prep -> handle_prep statevec prep
  ) in
  result
);;


(**
  * Runs a program, evaluating the quantum measurements using random functions.
  * First eval checks if the function is well formed. It then runs the program
  * `shots` times.
  *
  * If `shots == 0`, then the program is run once and the state vector resturned as is.
  * 
  * If `shots > 0`, then the program is run multiple times, read-out measurements are performed
  * on the vector collapsing each qubit to |+> or |->, and a probability distribution of the
  * results is returned.
  *)
let rand_eval ?(shots=0) ?(change_base=None) ?(qtbl=None) cmds = (
  Random.self_init();
  let default_basis = Qlib.Bases.z_basis in
  if well_formed cmds then (
    let cmds = expand_and_order_prep cmds in
    let qubit_num = calc_qubit_num cmds in
    let vec_size = Int.shift_left 1 qubit_num in
    let run_once () = (
      let init_statevec = Mat.make 1 1 Complex.one in
      let qtbl = (
        match qtbl with
        | None -> (
          let tbl = Hashtbl.create qubit_num in
          let _ = (
            List.iter (fun q -> Hashtbl.add tbl q q) (List.init qubit_num (fun x -> x))
          ) in tbl
        )
        | Some(qtbl) -> Hashtbl.copy qtbl
      ) in
      let mtbl = Hashtbl.create qubit_num in
      let exec_cmd' = rand_eval_cmd_exec mtbl qtbl in
      let statevec = List.fold_left (fun sv p -> (
        (*let _ = print_endline "HERE" in
        print_endline (cmd_to_string p);*)
        (*Mat.print sv;*)
        exec_cmd' sv p;
      )) init_statevec cmds in
      (qtbl, mtbl, Mat.cleanup statevec)
    ) in
    if shots == 0 then (
      (* Run weak simulation once; don't perform a read-out measurements. *)
      let (qtbl, mtbl, res) = run_once () in
      let qubit_num = (Hashtbl.length qtbl) in
      match change_base with
      | None -> (unpack_qtbl qtbl, get_readout mtbl, res)
      | Some(new_base) -> (
        (unpack_qtbl qtbl, get_readout mtbl, Qlib.StateVector.change_base default_basis new_base res qubit_num)
      )
    ) else (
      (* Run weak simulation `shots` times, performing a read-out measurement each time and averaging the results. *)
      let rec helper n qtbl mtbl sum = (
        if n > 0 then (
          (* Runs and applies read-out measurements to output qubits. *)
          let out_qubits = get_output_qubits cmds in
          let (qtbl, mtbl, res) = run_once () in
          let qubit_num = (Hashtbl.length qtbl) in
          (*let _ = Hashtbl.iter (fun q pos -> (
            print_endline ((Int.to_string q) ^ " " ^ (Int.to_string pos))
          )) qtbl in*)
          let measure = Qlib.StateVector.Measurement.measure ~proj_down:false (default_basis) qubit_num in
          let res' = (
            match change_base with
            | None -> res
            | Some(new_base) -> Qlib.StateVector.change_base default_basis new_base res qubit_num
          ) in
          let res'' = Hashtbl.fold (fun q _ vec -> (let (r, _) = measure q vec in r)) out_qubits (res') in
          helper (n-1) qtbl mtbl (Mat.add (Qlib.DensityMatrix.from_state_vector res'') sum)
        ) else (qtbl, mtbl, sum)
      ) in
      let (qtbl, mtbl, total) = helper shots (Hashtbl.create 0) (Hashtbl.create 0) (Mat.make vec_size vec_size Complex.zero) in
      let densemat = Mat.scal_mul (Cenv.(Complex.one / (c (Int.to_float shots) 0.))) total in
      (unpack_qtbl qtbl, get_readout mtbl, Mat.cleanup (Mat.from_col_vec (Mat.copy_diag densemat)))
    )
  ) else ([], Hashtbl.create 0, Mat.empty)
);;



(**********************************************************************************
  *                                                                               *
  *                         Simulate (Strong Simulation)                          *
  *                                                                               *
  *********************************************************************************)

(**
  * Performs appropriate operations to execute command.
  *)
let rec simulate_cmd_exec mtbl qtbl densmat cmds = (
  let getPos x = Hashtbl.find qtbl x in
  let qubit_num = (Hashtbl.length qtbl) in
  match cmds with
  | c::cmds -> (
    let open Qlib.DensityMatrix in
    let exec_cmd x = simulate_cmd_exec mtbl qtbl x cmds in
    match c with 
    | Entangle (qubit1, qubit2) -> (
      (* Entagles qubit1 and qubit2 by performing a controlled-Z operation *)
      (* qubit1 is the control *)
      let res = (
        apply_operator (ctrl_z qubit_num (getPos qubit1) (getPos qubit2)) densmat
      ) in
      exec_cmd res
    )
    | Measure (qubit, angle, signals_s, signals_t) -> (
      let open Qlib.DensityMatrix.Measurement in

      (* Calculates the measurement bases *)
      let bases = calc_measurement_states mtbl angle signals_s signals_t in
      let (case_1, case_2) = measure bases qubit_num (getPos qubit) densmat in

      let _ = update_qtbl qtbl qubit in

      let mtbl' = Hashtbl.copy mtbl in
      let qtbl' = Hashtbl.copy qtbl in
      let exec_cmd' x = simulate_cmd_exec mtbl' qtbl' x cmds in (
        Hashtbl.add mtbl qubit 0;
        Hashtbl.add mtbl' qubit 1;
        Mat.add (exec_cmd case_1) (exec_cmd' case_2)
      )
    )
    | XCorrect (qubit, signals) -> (
      let op = (pauli_x qubit_num (getPos qubit)) in
      let res = (
        if (List.length signals == 0) || (calc_signal mtbl signals > 0) then (
          apply_operator op densmat
        ) else densmat
      ) in exec_cmd res
    )
    | ZCorrect (qubit, signals) -> (
      let op = (pauli_z qubit_num (getPos qubit)) in
      let res = (
        if (List.length signals == 0) || (calc_signal mtbl signals > 0) then (
          apply_operator op densmat
        ) else densmat
      ) in exec_cmd res
    )
    | ReadOut (qubit, basis) -> (
      let open Qlib.DensityMatrix.Measurement in

      (* Calculates the measurement basis *)
      let bases = (
        match basis with
        | X -> Qlib.Bases.x_basis
        | Y -> Qlib.Bases.y_basis
        | Z -> Qlib.Bases.z_basis
        | Comp -> Qlib.Bases.z_basis
        | FromTuples(a, b) -> Qlib.Bases.from_tuples a b
        | FromAngle(a) -> Qlib.Bases.from_angle a
      ) in
      let (case_1, case_2) = measure bases qubit_num (getPos qubit) densmat in

      let _ = update_qtbl qtbl qubit in

      let mtbl' = Hashtbl.copy mtbl in
      let qtbl' = Hashtbl.copy qtbl in
      let exec_cmd' x = simulate_cmd_exec mtbl' qtbl' x cmds in (
        Hashtbl.add mtbl (-qubit - 1) 0;
        Hashtbl.add mtbl' (-qubit - 1) 1;
        Mat.add (exec_cmd case_1) (exec_cmd' case_2)
      )
    )
    | prep -> (
      let res = handle_prep ~densmat:true densmat prep in
      exec_cmd res
    )
  )
  | [] -> densmat
);;


(**
  * Runs a program, evaluating the operations using density matrices.
  * First eval checks if the function is well formed. It then it runs the program
  * and returns the resulting state vector.
  *
  * If `just_prob == true`, then just the probability distribution is returned.
  * The entire density matrix is returned otherwise.
  *)
let simulate ?(just_prob=false) ?(change_base=None) (cmds : prog) = (
  if well_formed cmds then (
    let cmds = expand_and_order_prep cmds in
    let qubit_num = calc_qubit_num cmds in
    let init_densmat = Mat.make 1 1 Complex.one in
    let qtbl = Hashtbl.create qubit_num in
    let _ = (
      List.iter (fun q -> Hashtbl.add qtbl q q) (List.init qubit_num (fun x -> x))
    ) in
    let mtbl = Hashtbl.create qubit_num in
    let exec_cmd' = simulate_cmd_exec mtbl qtbl in
    let densemat = exec_cmd' init_densmat cmds in
    let qubit_num = (Hashtbl.length qtbl) in
    let densemat' = (
      match change_base with
      | None -> densemat
      | Some(new_base) -> Qlib.DensityMatrix.change_base Qlib.Bases.z_basis new_base densemat qubit_num
    ) in
    if just_prob then (
      (unpack_qtbl qtbl, get_readout mtbl, Mat.from_col_vec (Mat.copy_diag densemat'))
    ) else (unpack_qtbl qtbl, get_readout mtbl, densemat')
  ) else ([], Hashtbl.create 0, Mat.empty)
);;


