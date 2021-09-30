
open Backend.Utils;;
open Backend.Types;;
open Backend.Run;;

open Algos.Create;;

open Lacamlext;;
open Lacaml.Z;;

let _ = (
  let b = Some(Qlib.Bases.z_basis, Qlib.Bases.x_basis) in
  let p = standardize ([InputList([
    (0, One);
    (1, One);
  ])] @ (qft [0; 1] 2)) in
  Mat.print (rand_eval ~shots:20 ~change_base:b p)
);;