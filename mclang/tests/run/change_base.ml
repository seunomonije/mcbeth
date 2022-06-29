
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
      let old_base = x_basis in
      let new_base = z_basis in
      Mat.print (change_base old_base new_base r 2)
    ) else Mat.print r
  )
);;

let () = foobar();;


(*
let foobar() = (
  print_endline("-- teleport test --");
  let p = (
    [Input(0, Plus); PrepList([1; 2])] @
    parse_pattern [J(0.0, 0, 1); J(0.0, 1, 2)];
  ) in (
    let _ = print_prog p in
    let qubit_num = calc_qubit_num p in
    let change = true in
    let r = rand_eval p in (
      if change then (
        let old_base = (plus_state, minus_state) in
        let new_base = (zero_state, one_state) in
        let new_mat = change_base old_base new_base r qubit_num in (
          Mat.print new_mat;
          extract_info ~print:true new_mat
        )
      ) else (
        Mat.print r;
        extract_info ~print:true r
      )
    )
  )
);;
*)
