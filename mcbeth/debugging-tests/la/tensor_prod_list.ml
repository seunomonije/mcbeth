open Lacamlext;;
open Lacaml.Z;;

open Format;;
open Lacaml.Io;;

let () = let open Cenv in (
  let a1 = Mat.of_array [|
    [| c 1. 0.; c 2. 0. |];
    [| c 0. 0.; c (1.) 0. |];
  |] in
  let a2 = Mat.of_array [|
    [| c 2. 0.; c 3. 1.5 |];
    [| c 1. 2.; c (-.5.) 0. |];
  |] in (
    printf "a1 = @[%a@]@\n@\n" pp_cmat a1;
    printf "a2 = @[%a@]@\n@\n" pp_cmat a2;
    printf "a1 (x) a2 = @[%a@]@\n@\n" pp_cmat (Mat.tensor_prod_list [a1; a2]);
    printf "a1 (x) a1 (x) a2 = @[%a@]@\n@\n" pp_cmat (Mat.tensor_prod_arr [|a1; a1; a2|])
  );
  let v1 = Vec.of_array [| c 1. 0.; c 2. 0.; c 3. 0.; |] in
  let v2 = Vec.of_array [| c 1. 0.; c 2. 0.; |] in (
    printf "v1 = @[%a@]@\n@\n" pp_cvec v1;
    printf "v2 = @[%a@]@\n@\n" pp_cvec v2;
    printf "v1 (x) v2 = @[%a@]@\n@\n" pp_cvec (Vec.tensor_prod_arr [|v1; v2|]);
    printf "v1 (x) v2 (x) v2 = @[%a@]@\n@\n" pp_cvec (Vec.tensor_prod_list [v1; v2; v2])
  )
);;
