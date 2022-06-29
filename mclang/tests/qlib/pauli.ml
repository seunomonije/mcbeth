
open Qlib.Gates;;

open Lacamlext;;
open Lacaml.Z;;

let () = (
  Mat.print (single_pauli_x);
  Mat.print (single_pauli_y);
  Mat.print (single_pauli_z);
  Mat.print (pauli_y 2 1);
  Mat.print (pauli_z 3 2)
)
