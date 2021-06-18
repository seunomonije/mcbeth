(**
  * Density Matrix Implementation of Comamnds
  *)


open Types;;
open Utils;;

open Lacamlext;;
open Lacaml.Z;;


(**********************************************************************************
  *                                                                               *
  *                           Randomized Evaluation                               *
  *                                                                               *
  *********************************************************************************)


(**
  *  Performs appropriate operations to execute command.
  *)
let eval_cmd (densitymat : Mat.t) (c : cmd) : Mat.t = (
  match c with 
  | Entangle (qubit1, qubit2) -> (
    (* Logic if the state if control is in 1 or 0, do the following: *)
    (* states.(qubit1) <- mat_scal_mul control_z qubit_1 *)
    (* states.(qubit2) <- mat_scal_mul control_z qubit_2*)
    let _ = (qubit1, qubit2) in densitymat
  )
  | Measure (qubit, angle, signals_s, signals_t) -> (
    (* Measuring at this point should be nothing more than a signal or
    flag that we add to the end, we can actually preform the calculations
    at the end after we have generated the map of states. *)
    let _ = (qubit, angle, signals_s, signals_t) in densitymat
  )
  | XCorrect (qubit, signals) -> (
    (* Multiplying a scalar by a vector generates a matrix, so we 
    will need to adjust our array to type matrix and evaluate
    as such.*)
    (* states.(qubit) <- mat_scal_mul states.(qubit) *) 
    let _ = (qubit, signals) in densitymat
  )
  | ZCorrect (qubit, signals) -> (
    (* states.(qubit) <- mat_scal_mul states.(qubit) *)
    let _ = (qubit, signals) in densitymat
  )
);;

let eval_cmds densitymat cmds = (
  List.fold_left (fun dmat p -> eval_cmd dmat p) densitymat cmds
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
      let init_densitymat = (* TODO *) Mat.empty in
      let densitymat = eval_cmds init_densitymat cmds in (
        let _ = (qubit_inits, densitymat) in (* this line is just to remove unused variable errors *)
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




