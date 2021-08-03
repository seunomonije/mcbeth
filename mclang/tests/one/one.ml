
open Backend.Types;;
open Backend.Run;;
open Backend.Utils;;

open Lacamlext;;
open Lacaml.Z;;

open Qlib.DensityMatrix;;
open Qlib.States;;

let foobar() = (
  print_endline("-- teleport test --");
  let p = (
    [Input(0, Minus); PrepList([1; 2])] @
    parse_pattern [J(0.0, 0, 1); J(0.0, 1, 2)];
  ) in (
    let _ = print_prog p in
    let qubit_num = calc_qubit_num p in
    let foo = true in
    let r = simulate p in (
      if foo then (
        let old_base = (plus_state, minus_state) in
        let new_base = (zero_state, one_state) in
        let new_mat = change_base old_base new_base r qubit_num in (
          Mat.print new_mat;
          let _ = extract_info ~print:true new_mat in ();
        )
      ) else (
        Mat.print r;
        let _ = extract_info ~print:true r in ()
      )
    )
  )
);;

let () = foobar();;


