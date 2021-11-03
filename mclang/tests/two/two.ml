
open Lacaml.D;;

open Backend.Utils;;


let foobar() = (
  let k = 15 in
  let k' = Int.shift_left 1 k in
  let b = Mat.make k' k' 1. in
  let t = performance (gemm b) b in
  print_endline (Float.to_string t)
);;

let _ = foobar();;


