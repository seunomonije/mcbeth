
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
  | Measure of qubit * float * qubit list * qubit list
  | XCorrect of qubit * qubit list
  | ZCorrect of qubit * qubit list

type prog = prep list * cmd list
