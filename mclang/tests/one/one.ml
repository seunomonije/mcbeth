
open Backend.Types;;
open Backend.Run;;
open Backend.Utils;;

open Lacamlext;;
open Lacaml.Z;;

open Qlib.DensityMatrix;;
open Qlib.States;;

let foobar() = (
  print_endline("-- foobar test --");
  let p = (
    [Input(0, Zero); PrepList([1; 2])] @
    parse_pattern [J(0.0, 0, 1); J(0.0, 1, 2)];
  ) in (
    let _ = print_prog p in
    let foo = false in
    let r = simulate p in (
      if foo then (
        let old_base = (plus_state, minus_state) in
        let new_base = (zero_state, one_state) in
        Mat.print (change_base old_base new_base r 2)
      ) else Mat.print r;
    )
  )
);;

let () = foobar();;


