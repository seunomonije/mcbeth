open Lacamlext;;
open Lacaml.Z;;

open Format;;
open Lacaml.Io;;

let () = let open Cenv in (
  let a = Mat.of_array [|
    [| c 2. 0.; c 3. 1.5 |];
    [| c 1. 2.; c (-.5.) 0. |];
  |] in
  let a' = Mat.scal_mul (c 2. 0.) a in (
    printf "a = @[%a@]@\n@\n" pp_cmat a;
    printf "a' = @[%a@]@\n@\n" pp_cmat a';
  )
);;
