
(*
open Lacaml.D;;
open Backend.Utils;;
*)

open Backend.Distributed;;
open Backend.Types;;

open Algos.Create;;

let foobar() = (
  let p = (
    [Input(0, One); Input(1, Zero); Prep(2)] @ (teleport 0 1 2);
  ) in
  let d = build_dist_map [
    [0;2];
    [1];
  ] in
  let nl = build_node_loc [
    ("localhost", 9000);
    ("localhost", 9001);
  ] in
  let dp = build_dist_prog d p in
  let da = build_dist_approx dp in (
    print_dist_prog dp;
    print_dist_approx da;
    run_dist_approx da nl
  )
);;

(*
let foobar() = (
  let k = 15 in
  let k' = Int.shift_left 1 k in
  let b = Mat.make k' k' 1. in
  let t = performance (gemm b) b in
  print_endline (Float.to_string t)
);;
*)
let _ = foobar();;


