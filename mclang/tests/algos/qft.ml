(*
open Backend.Utils;;
open Backend.Types;;
open Backend.Run;;

open Algos.Create;;

open Lacamlext;;
open Lacaml.Z;;

let _ = (
  let _ = Some(Qlib.Bases.z_basis, Qlib.Bases.x_basis) in
  let p = standardize ([CInputList([
    (0, One);
    (1, One);
  ])] @ (qft [0; 1] 2)) in
  Mat.print (simulate ~change_base:None p)
);;
*)

(* WORK IN PROGRESS
open Backend.Types;;
open Backend.Utils;;

let qft (transform_qubits : qubit list) (total_qubits : int) = (
  let phase_angle m = (
    Float.div 
      (Float.mul 2. (Float.pi)) 
      (Int.to_float (Int.shift_left 1 m))
  ) in
  let build_cp_chain q qs n = (
    let helper (chain, m, n) c = (
      let pat = CP(phase_angle m, c, n, n+1, n+2, n+3, n+4, n+5, n+6, n+7, q) in
      (chain @ [pat], m+1, n+8)
    ) in
    let (chain, _, n') = List.fold_left helper ([], 2, n) qs in (chain, n')
  ) in
  let rec handle_qubit qubits n = (
    match qubits with
    | []    -> ([], n)
    | q::qs -> (
      let (cp_chain, n') = build_cp_chain q qs (n+1) in
      let (rest, n'') = handle_qubit qs n' in
      ([H(n, q)] @ cp_chain @ rest, n'')
    )
  ) in
  let (pat, n) = handle_qubit transform_qubits total_qubits in
  let prep_cmd = PrepList(List.init (n - total_qubits) (fun x -> x + total_qubits)) in
  let pat' = [CMD(prep_cmd)] @ pat in
  parse_pattern pat'
);;
*)