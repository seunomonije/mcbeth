
open Backend.Types;;
open Backend.Run;;

open Lacamlext;;
open Lacaml.Z;;

let foobar() = (
  print_endline("-- foobar test --");
  let p = ([Prep(0); Prep(1);
            Entangle(0, 1); Measure(0, 0.0, [], [])]) 
  in (
    let statevec = rand_eval p in Vec.print statevec
  )
);;

let () = foobar();;


