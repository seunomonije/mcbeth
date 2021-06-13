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
  *  Performs appropriate operations to execute command.
  *)
let eval_cmd qubit_num (statevec : Vec.t) (c : cmd) : unit = (
  match c with 
  | Entangle (qubit1, qubit2) -> (
    (* Entagles qubit1 and qubit2 by performing a controlled-Z operation *)
    (* qubit1 is the control *)
    ctrl_z qubit_num qubit1 qubit2
  )
  | Measure (qubit, angle, signals_s, signals_t) -> (
    (* Measuring at this point should be nothing more than a signal or
    flag that we add to the end, we can actually preform the calculations
    at the end after we have generated the map of states. *)
  )
  | XCorrect (qubit, signals) -> (
    (* Multiplying a scalar by a vector generates a matrix, so we 
    will need to adjust our array to type matrix and evaluate
    as such.*)
    (* states.(qubit) <- mat_scal_mul states.(qubit) *) 
  )
  | ZCorrect (qubit, signals) -> (
    (* states.(qubit) <- mat_scal_mul states.(qubit) *)
  )
);;

let eval_cmds statevec cmds = (
  List.fold_left (fun svec p -> eval_cmd svec p) statevec cmds
);;


(**
  *  Runs a program, evaluating the quantum measurements using random functions.
  *  First eval checks if the function is well formed. It then it runs the program
  *  and returns the result extracted from the state matrix.
  *)
let eval ((preps, cmds) as p : prog) : bool list = (
  let qubit_num = well_formed(p) in (
    if qubit_num > 0 then (
      let qubit_inits = prep_qubits qubit_num preps in
      let init_statevec = Vec.tensor_prod_arr qubit_inits in
      let statevec = eval_cmds qubit_num init_statevec cmds in (
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


