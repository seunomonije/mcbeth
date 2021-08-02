
open Backend.Types;;
open Backend.Run;;
open Backend.Utils;;

open Lacamlext;;
open Lacaml.Z;;

open Qlib.StateVector;;
open Qlib.States;;

let foobar() = (
  print_endline("-- foobar test --");
  let p = (
    [Input(0, Plus); PrepList([1; 2])] @
    parse_pattern [J(0.0, 0, 1); J(0.0, 1, 2)];
  ) in (
    let _ = print_prog p in
    let qubit_num = calc_qubit_num p in
    let foo = true in
    let r = rand_eval p in (
      if foo then (
        let old_base = (plus_state, minus_state) in
        let new_base = (zero_state, one_state) in
        Mat.print (change_base old_base new_base r qubit_num)
      ) else Mat.print r;
    )
  )
);;

let () = foobar();;


