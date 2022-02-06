
open Backend.Types;;
open Backend.Utils;;

let teleport qubit1 qubit2 ancillary = (
  parse_pattern [J(0.0, qubit1, ancillary); J(0.0, ancillary, qubit2)]
);;
