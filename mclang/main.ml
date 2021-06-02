(*
 *  Measurement Calculus Programming Language
 *
 *  Date: May 2021
 *  Authors: Aidan Evans and Seun Omonije
 *)


(**********************************************************************************
 *                                                                                *
 *                              Data Type Declarations                            *
 *                                                                                *
 **********************************************************************************)

type qubit = int

type prep = 
  | Init of qubit * float
  | Init0 of qubit
  | Init1 of qubit
  | InitPlus of qubit
  | InitMinus of qubit
  | InitNonInput of qubit list

type cmd =
  | Entangle of qubit * qubit
  | Measure of qubit * float * int * int
  | XCorrect of qubit * int
  | ZCorrect of qubit * int

type prog = prep list * cmd list


(**********************************************************************************
 *                                                                                *
 *                                Utility Functions                               *
 *                                                                                *
 **********************************************************************************)

(*
 *  Checks whether a program is well formed, i.e., whether it satisfies
 *  the following conditions:
 *    (D0) No command depends on an outcome not yet measured.
 *    (D1) No command acts on a qubit already measured.
 *    (D2) No command acts on a qubit not yet prepared, unless it is an input qubit.
 *    (D3) A qubit i is measured if and only if i is not an output.
 *)(*
let well_formed (p : prog) : bool = (
  
);;*)

(*
 *  Converts numbers to a strings
 *)
let to_string = Int.to_string;;
let float_to_string x = Printf.sprintf "%.5f" x;;

(*
 *  Converts a preperation to a friendly, human-readable string.
 *)
let prep_to_string p = (
  match p with
  | Init (qubit, base_angle) -> 
    "Init[" ^ to_string qubit ^ ", " ^ float_to_string base_angle ^ "]"
  | Init0 (qubit) -> 
    "Init0[" ^ to_string qubit ^ "]"
  | Init1 (qubit) ->
    "Init1[" ^ to_string qubit ^ "]"
  | InitPlus (qubit) ->
    "Init+[" ^ to_string qubit ^ "]"
  | InitMinus (qubit) ->
    "Init-[" ^ to_string qubit ^ "]"
  | InitNonInput (qubits) ->
    "InitNonInput[" ^ (String.concat ", " (List.map to_string qubits)) ^ "]"
);;
  
(*
 *  Converts a command to a friendly, human-readable string.
 *)
let cmd_to_string c = (
  match c with 
  | Entangle (left, right) ->
    "E[" ^ to_string left ^ ", " ^ to_string right ^ "]"
  | Measure (qubit, angle, parity1, parity2) ->
    "M[" ^ String.concat ", " [to_string qubit; float_to_string angle; to_string parity1; to_string parity2] ^ "]"
  | XCorrect (qubit, signal) ->
    "X[" ^ to_string qubit ^ ", " ^ to_string signal ^ "]"
  | ZCorrect (qubit, signal) ->
    "Z[" ^ to_string qubit ^ ", " ^ to_string signal ^ "]"
);;

(*
 *  Prints out the program in a friendly, human-readable format.
 *)
let print_prog ((preps, cmds) : prog) = (
  List.iter (fun x -> print_endline(prep_to_string(x))) preps;
  List.iter (fun x -> print_endline(cmd_to_string(x))) cmds
);;


(**********************************************************************************
 *                                                                                *
 *                              Evaluation Functions                              *
 *                                                                                *
 **********************************************************************************)
(*
(* Randomized *)
let eval ((preps, cmds) : prog) : bool list = (
  
);;
*)

print_prog ([Init(1, 0.375324); InitNonInput([2; 3])], [Entangle(1, 2)]);;

