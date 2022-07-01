
open Backend.Types;;

let g2 q1 q2 next_qubit (a, b) = (
  let n = next_qubit in
  let (alpha, beta) = (
    match (a, b) with
    | (0, 0) -> (Float.pi, Float.pi)
    | (0, 1) -> (Float.pi, 0.0)
    | (1, 0) -> (0.0, Float.pi)
    | (1, 1) -> (0.0, 0.0)
    | _ -> (0.0, 0.0) (* assumed not to reach *)
  ) in [
    PrepList([q1; q2; n; n+1;]);
    
    CZ(n, n+1);
    J(alpha, n, q1);
    J(beta, n+1, q2);
    CZ(q1, q2);
    ReadOut(q1, FromAngle(Float.pi));
    ReadOut(q2, FromAngle(Float.pi))

    (* FOR TESTING OTHER VARIATION
    Measure(q1, -.Float.pi, [], []);
    Measure(q2, -.Float.pi, [], []);
    J(-.Float.pi, n+2, q1);
    J(-.Float.pi, n+3, q2);
    *)
  ]
);;
