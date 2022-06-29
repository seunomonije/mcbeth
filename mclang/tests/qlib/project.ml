
open Qlib.States;;
open Qlib.StateVector.Measurement;;

open Lacamlext;;
open Lacaml.Z;;

let () = (
  Mat.print (project (Mat.from_col_vec zero_state_vec));
  Mat.print (project (Mat.from_col_vec one_state_vec))
)
