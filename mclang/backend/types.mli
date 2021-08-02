
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
  | J of float * qubit * qubit
  | Z of qubit * qubit

type pattern = pattern_part list
