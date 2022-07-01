
open Format;;

open Lacaml.Z;;
open Lacaml.Io;;

open Cenv;;


let () = (
  let a =
    Mat.of_array
      [|
        [| c 2. 0.; c 3. 1.5 |];
        [| c 1. 2.; c (-.5.) 0. |];
      |] 
  in
  printf "a = @[%a@]@\n@\n" pp_cmat a
)