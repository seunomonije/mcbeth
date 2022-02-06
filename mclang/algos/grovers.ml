
open Backend.Types;;
open Backend.Utils;;

let g2 q1 q2 next_qubit (a, b) = (
  let n = next_qubit in
  let (alpha, beta) = (
    match (a, b) with
    | (0, 0) -> (Float.pi, Float.pi)
    | (0, 1) -> (Float.pi, 0.0)
    | (1, 0) -> (0.0, Float.pi)
    | (1, 1) -> (0.0, 0.0)
    | _ -> (0.0, 0.0) (* assumed not to reach *)
  ) in parse_pattern [
    CMD(PrepList([n; n+1;]));
    
    CZ(n, n+1);
    J(alpha, n, q1);
    J(beta, n+1, q2);
    CZ(q1, q2);
    CMD(ReadOut(q1, FromAngle(Float.pi)));
    CMD(ReadOut(q2, FromAngle(Float.pi)))

    (*
    CMD(Measure(q1, -.Float.pi, [], []));
    CMD(Measure(q2, -.Float.pi, [], []));
    J(-.Float.pi, n+2, q1);
    J(-.Float.pi, n+3, q2);
    
    *)
  ]
);;
(*
let ccz q1 q2 q3 next_qubit = (
  let n = next_qubit in
  parse_pattern [
    CMD(PrepList([  ]));


  ]
)

let g3 q1 q2 q3 next_qubit (a, b, c) = (

)
*)