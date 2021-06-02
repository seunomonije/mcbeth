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

  | Entangle (left, right) ->
    "E[" ^ to_string left ^ ", " ^ to_string right ^ "]"
  | XCorrect (value) ->
    "X[" ^ to_string value ^ "]"
  | ZCorrect (value) ->
    "Z[" ^ to_string value ^ "]"
  | Value v -> v;;

let print_expr e = 
  print_endline (to_string e);;


print_expr (XCorrect ((Value "1")));;