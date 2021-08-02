
open Backend.Types;;
open Backend.Run;;

open Lacamlext;;
open Lacaml.Z;;

open Qlib.StateVector;;
open Qlib.States;;

let foobar() = (
  print_endline("-- foobar test --");
  let p = ([Input(0, Plus); (*Input(1, Plus);*)
            (*Entangle(0, 1); Measure(0, 0.0, [], [])*)]) 
  in (
    let r = rand_eval p in
    let old_base = (plus_state)
    Mat.print (change_base)
  )
);;

let () = foobar();;


