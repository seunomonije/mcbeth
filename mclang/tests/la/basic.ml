
open Format;;

open Lacaml.D;;
open Lacaml.Io;;

let () = (
  let a =
    Mat.of_array
      [|
        [| 2.; 3. |];
        [| 1.; -5. |];
      |] 
  in
  let b =
    Mat.of_array
      [|
        [| 4.; 3.; 6. |];
        [| 1.; -2.; 3. |];
      |] 
  in
  printf "a = @[%a@]@\n@\n" pp_fmat a;
  printf "b = @[%a@]@\n@\n" pp_fmat b
)