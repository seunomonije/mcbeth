open Lacamlext;;
open Lacaml.Z;;

open Format;;
open Lacaml.Io;;

let () = let open Cenv in (
  let t = Mat.of_array [|
    [| c 1. 0.; c 2. 0. |];
    [| c 0. 0.; c (1.) 0. |];
  |] in
  let a = Mat.of_array [|
    [| c 2. 0.; c 3. 1.5 |];
    [| c 1. 2.; c (-.5.) 0. |];
  |] in (
    printf "t = @[%a@]@\n@\n" pp_cmat t;
    printf "a = @[%a@]@\n@\n" pp_cmat a;
    printf "t (x) a = @[%a@]@\n@\n" pp_cmat (Mat.tensor_prod t a)
  );
  let v1 = Vec.of_array [| c 1. 0.; c 2. 0.; c 3. 0.; |] in
  let v2 = Vec.of_array [| c 1. 0.; c 2. 0.; |] in
  let t = Vec.tensor_prod v1 v2 in (
    printf "v1 = @[%a@]@\n@\n" pp_cvec v1;
    printf "v2 = @[%a@]@\n@\n" pp_cvec v2;
    printf "v1 (x) v2 = @[%a@]@\n@\n" pp_cvec t;
  )
);;
