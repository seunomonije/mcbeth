
open Qlib.Gates;;

open Lacamlext;;
open Lacaml.Z;;

let () = (
  Mat.print (ctrl_x 2 0 1);
  Mat.print (ctrl_x 3 0 2);
  Mat.print (ctrl_x 3 2 0)
)
