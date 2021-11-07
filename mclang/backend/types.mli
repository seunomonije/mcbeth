
type qubit = int

type input = 
  | Zero 
  | One 
  | Plus 
  | Minus
  | State of Complex.t * Complex.t

type cmd =
  | Prep of qubit
  | Input of qubit * input
  | PrepList of qubit list
  | InputList of (qubit * input) list
  | Entangle of qubit * qubit
  | Measure of qubit * float * qubit list * qubit list
  | XCorrect of qubit * qubit list
  | ZCorrect of qubit * qubit list

type prog = cmd list

type pattern_part = 
  | J of float * qubit * qubit  (* J generator *)
  | CZ of qubit * qubit         (* Controlled-Z gate *)
  | H of qubit * qubit          (* Hadamard gate *)
  | CX of qubit * qubit * qubit * qubit (* Controlled-X / CNOT *)
  | CNOT of qubit * qubit * qubit * qubit (* Controlled-X / CNOT *)
  | RX of float * qubit * qubit * qubit (* X Rotation *)
  | RZ of float * qubit * qubit * qubit (* Z Rotation *)
  | P of float * qubit * qubit * qubit (* Phase gate *)
  | CP of float * qubit * qubit * qubit * qubit
                * qubit * qubit * qubit * qubit
                * qubit * qubit (* Controlled Phase gate *)
  | CP2 of float * qubit * qubit * qubit * qubit * qubit * qubit
  | CMD of cmd

type pattern = pattern_part list

type qtbl = (qubit, int) Hashtbl.t

(* Types used for distributed computation *)
type dist_map = (int, qtbl) Hashtbl.t

type dist_struct = (int, qtbl * prog) Hashtbl.t
type dist_prog = prog * dist_struct

type dist_approx = dist_struct

type node_locations = (int, string * int) Hashtbl.t
