
open Backend.Types;;
open Backend.Run;;

open Lacamlext;;
open Lacaml.Z;;

let foobar() = (
  print_endline("-- foobar test --");
  let p = ([Prep(0); Input(1, Plus);
            Entangle(0, 1); Measure(0, 0.0, [], [])]) 
  in (
    let densmat = simulate p in Mat.print densmat
  )
);;

let () = foobar();;


