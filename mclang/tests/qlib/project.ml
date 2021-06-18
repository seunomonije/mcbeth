
open Qlib.States;;
open Qlib.Measurement;;

open Lacamlext;;
open Lacaml.Z;;

let () = (
  Mat.print (project (Mat.from_col_vec zero_state));
  Mat.print (project (Mat.from_col_vec one_state))
)
