
open Backend.Types;;
open Backend.Utils;;

let g2 q1 q2 total_qubits (a, b) = (
  let n = total_qubits in
  let (alpha, beta) = (
    match (a, b) with
    | (0, 0) -> (Float.pi, Float.pi)
    | (0, 1) -> (Float.pi, 0.0)
    | (1, 0) -> (0.0, Float.pi)
    | (1, 1) -> (0.0, 0.0)
    | _ -> (0.0, 0.0) (* assumed not to reach *)
  ) in parse_pattern [
    CMD(PrepList([n; n+1; n+2; n+3]));
    
    CZ(n, n+1);
    J(alpha, n, n+2);
    J(beta, n+1, n+3);

    CZ(n+2, n+3);
    J(Float.pi, n+2, q1);
    J(Float.pi, n+3, q2);
  ]
);;
