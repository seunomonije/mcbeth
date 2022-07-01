
open Backend.Types;;

let teleport qubit1 qubit2 qubit3 = (
  [Input(qubit1); PrepList([qubit2; qubit3]); J(0.0, qubit1, qubit2); J(0.0, qubit2, qubit3)]
);;
