
open Backend.Types;;
open Backend.Utils;;

open Backend.Run;;

open Lacamlext;;
open Lacaml.Z;;

open Algos.Create;;
(*
*)

let foobar() = (
  let p = (
    (*
    parse_pattern 
    [CMD(CInputList([(0, Plus); (1, One)])); CMD(PrepList([2; 3; 4; 5]));
                    CP2(Float.div Float.pi 2., 1, 2, 3, 4, 5, 0);]
    [CInputList([(0, One); (1, One);])] @ (qft [0; 1] 2);
    [
      CInputList([(0, Plus); (1, Plus)]);
      PrepList([2; 3]);
      Entangle(0, 1);
      Entangle(2, 0);
      Entangle(3, 1);
      Measure(2, -0.0, [], []);
      Measure(3, -0.0, [], []);
    ]
    
    [CInputList([(0, Plus); (1, Plus)]); Entangle(0, 1);]
    [CInputList([(0, One); (1, Zero)]); PrepList([2; 3; 4; 5; 6; 7; 8; 9;]); 
    
    ] @ parse_pattern [
      CP(Float.pi, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    ] 
    *)
    (grover2 0 1 2 (0, 1));
    
  ) in (
    let inputs = [(0, Plus); (1, Plus);] in
    print_prog p; print_endline "";
    let _ = Some(Qlib.Bases.from_angle Float.pi) in
    if well_formed p then print_endline "yay";
    let p' = standardize p in (
      print_prog p';
      let qs, readout, res = run (Weak(0, None, None)) p' inputs in (
        print_qubits qs;
        print_readout readout;
        Mat.print res
      )
    (*
    *)
    )
  )
)

(*
let foobar() = (
  let b = Some(Qlib.Bases.y_basis) in
  let p = (
    (*[CInput(0, Zero); PrepList([1; 2])] @
    parse_pattern [J(0.0, 0, 1); J(0.0, 1, 2)];*)
    (*[CInput(0, Zero); Prep(1); Measure(0, 0.0, [], []); ZCorrect(1, [])]*)
    (*[PrepList([0; 1]); Entangle(0, 1);  Measure(0, 0.0, [], []); XCorrect(1, [0])]*)
    parse_pattern [CMD(CInputList([(0, Minus);])); CMD(PrepList([1; 2]));
                    P(Float.div Float.pi 2., 1, 2, 0);]
  ) in (
    let p' = standardize p in (
      print_prog (expand_and_order_prep p');
      Mat.print (rand_eval ~shots:10 ~change_base:b p');
      print_endline "huh";
      Mat.print (simulate ~just_prob:true ~change_base:b p');
    )
  )
);;
*)
let _ = foobar();;


