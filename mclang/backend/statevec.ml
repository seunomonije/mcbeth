(**
  * Statevector Implementation of Comamnds
  *)


open Types;;
open Utils;;

open Lacamlext;;
open Lacaml.Z;;

open Qgates;;



(**********************************************************************************
  *                                                                               *
  *                           Randomized Evaluation                               *
  *                                                                               *
  *********************************************************************************)

(**
  * Given a state vector, returns the outcome of a qubit `q`.
  * Assumes that the qubit has already been measured.
  *)
let get_outcome (statevec : Mat.t) (q : qubit) : int = (
  let _ = (statevec, q) in 0
)


(**
  * Performs appropriate operations to execute command.
  *)
let eval_cmd qubit_num (statevec : Mat.t) (c : cmd) : Mat.t = (
  match c with 
  | Entangle (qubit1, qubit2) -> (
    (* Entagles qubit1 and qubit2 by performing a controlled-Z operation *)
    (* qubit1 is the control *)
    gemm (ctrl_z qubit_num qubit1 qubit2) statevec
  )
  | Measure (qubit, angle, signals_s, signals_t) -> (
    (* Measuring at this point should be nothing more than a signal or
    flag that we add to the end, we can actually preform the calculations
    at the end after we have generated the map of states. *)
  )
  | XCorrect (qubit, signals) -> (
    
  )
  | ZCorrect (qubit, signals) -> (

  )
);;

let eval_cmds qubit_num statevec cmds = (
  let eval_cmd' = eval_cmd qubit_num in
  List.fold_left (fun svec p -> eval_cmd' svec p) statevec cmds
);;


(**
  * Runs a program, evaluating the quantum measurements using random functions.
  * First eval checks if the function is well formed. It then it runs the program
  * and returns the result extracted from the state matrix.
  *)
let eval ((preps, cmds) as p : prog) : bool list = (
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


let foobar() = (
  print_endline("-- foobar test --");
  let p = ([Init0(0); Init1(1); InitMinus(2); Init(3, 0.375324); InitNonInput([4; 5])], 
            [Entangle(1, 0); Measure(1, 0.0, [], []); XCorrect(0, [])]) 
  in let _ = eval p in ()
);;


