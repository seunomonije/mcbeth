
open Backend.Utils;;
open Backend.Types;;
open Backend.Run;;

open Algos.Create;;

open Lacamlext;;
open Lacaml.Z;;

let _ = (
  let _ = Some(Qlib.Bases.z_basis, Qlib.Bases.x_basis) in
  let p = standardize ([CInputList([
    (0, One);
    (1, One);
  ])] @ (qft [0; 1] 2)) in
  Mat.print (simulate ~change_base:None p)
);;