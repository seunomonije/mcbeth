
open Backend.Types;;
open Backend.Run;;

open Lacamlext;;
open Lacaml.Z;;

open Qlib.DensityMatrix;;
open Qlib.Bases;;

let foobar() = (
  print_endline("-- foobar test --");
  let p = ([Input(0, Minus); Input(1, Plus);
            (*Entangle(0, 1); Measure(0, 0.0, [], [])*)]) 
  in (
    let foo = true in
    let r = simulate p in
    if foo then (
      let old_base = x_bases in
      let new_base = z_bases in
      Mat.print (change_base old_base new_base r 2)
    ) else Mat.print r
  )
);;

let () = foobar();;


