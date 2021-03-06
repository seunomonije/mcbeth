
open Backend.Types;;

open Algos.Create;;

let programs = [

  [Prep(0); CInput(1, One)];

  [Prep(0); Prep(1);
  Entangle(0, 1); Measure(0, 0.0, [], [])];

  [Prep(0); Prep(1);
  Entangle(0, 1); Measure(0, 0.0, [], []); Measure(1, 0.0, [], [])];

  (*
  parse_pattern 
  [CMD(CInputList([(0, Plus);])); CMD(PrepList([1; 2]));
  P(Float.div Float.pi 2., 1, 2, 0);];
  *)

  [PrepList([0; 1; 2; 3; 4]);
  Entangle(0, 1); Entangle(1, 2); Entangle(2, 3);
  Measure(0, 0.0, [], []); Measure(1, 0.0, [0], []);
  Measure(2, 3.14159, [0; 1], []); Measure(3, 0.0, [0], [2]);
  XCorrect(4, [3])];
  
  [PrepList([0; 1; 2; 3; 4; 5; 6; 8; 9;]); 
  CInput(7, One);
  Entangle(0, 1); Entangle(1, 2); Entangle(2, 3);
  Measure(0, 0.0, [], []); Measure(1, 0.0, [0], []);
  Measure(2, 3.14159, [0; 1], []); Measure(3, 0.0, [0], [2]);
  Measure(4, 3.14159, [0; 1; 3], []); Measure(6, 0.0, [4], [2]);
  Measure(5, 3.14159, [], []); Measure(7, 0.0, [], [6; 5]);
  XCorrect(8, [7; 2; 3]);
  ZCorrect(9, [5; 6; 2])];

  (* Teleport *)
  teleport 0 1 2;
  
  (* QFT on 2 qubits 
  [CInputList([(0, One); (1, One);])] @ (qft [0; 1] 2);*)
  
];;
