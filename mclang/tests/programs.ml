
open Backend.Types;;
open Backend.Utils;;
open Backend.Distributed;;

open Algos.Create;;

let programs = [

  [Prep(0); Input(1, One)];

  [Prep(0); Prep(1);
  Entangle(0, 1); Measure(0, 0.0, [], [])];

  [Prep(0); Prep(1);
  Entangle(0, 1); Measure(0, 0.0, [], []); Measure(1, 0.0, [], [])];

  parse_pattern 
  [CMD(InputList([(0, Plus);])); CMD(PrepList([1; 2]));
  P(Float.div Float.pi 2., 1, 2, 0);];

  [PrepList([0; 1; 2; 3; 4]);
  Entangle(0, 1); Entangle(1, 2); Entangle(2, 3);
  Measure(0, 0.0, [], []); Measure(1, 0.0, [0], []);
  Measure(2, 3.14159, [0; 1], []); Measure(3, 0.0, [0], [2]);
  XCorrect(4, [3])];
  
  [PrepList([0; 1; 2; 3; 4; 5; 6; 8; 9;]); 
  Input(7, One);
  Entangle(0, 1); Entangle(1, 2); Entangle(2, 3);
  Measure(0, 0.0, [], []); Measure(1, 0.0, [0], []);
  Measure(2, 3.14159, [0; 1], []); Measure(3, 0.0, [0], [2]);
  Measure(4, 3.14159, [0; 1; 3], []); Measure(6, 0.0, [4], [2]);
  Measure(5, 3.14159, [], []); Measure(7, 0.0, [], [6; 5]);
  XCorrect(8, [7; 2; 3]);
  ZCorrect(9, [5; 6; 2])];

  (* Teleport *)
  [Input(0, One); Input(1, Plus); Prep(2)] @ (teleport 0 1 2);
  
  (* QFT on 2 qubits *)
  [InputList([(0, One); (1, One);])] @ (qft [0; 1] 2);
  
];;

(**
  * Generates a random qubit by performing n random rotations,
  * uses n+1 qubits
  *)
let linear ?(offset=0) n = (
  Random.self_init();
  let next () = Random.float (Float.mul 2. Float.pi) in
  let prep = PrepList(List.init (n+1) (fun x -> x + offset)) in
  let cmds = List.fold_left (fun ls i -> (
    ls @ (parse_pattern [J(next(), i, i+1)])
  )) [] (List.init n (fun x -> x + offset)) in
  standardize (prep::cmds)
);;

let parallel ?(entangle=false) n m = (
  let _ = entangle in
  let cmds = List.fold_left (fun ls o -> (
    ls @ (linear n ~offset:o)
  )) [] (List.init m (fun x -> x*(n+1))) in
  standardize cmds
);;

let distributed ?(entangle=false) n m = (
  let prog = parallel n m ~entangle:entangle in
  let dist = build_dist_map (List.fold_left (fun ls o -> (
    (List.init (n+1) (fun x -> x + o)) :: ls
  )) [] (List.init m (fun x -> x*(n+1)))) in
  build_dist_prog dist prog
);;

let distributed_approx ?(entangle=false) n m = (
  build_dist_approx (distributed n m ~entangle:entangle)
);;
