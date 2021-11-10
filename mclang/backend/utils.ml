
open Types;;

(*
open Format;;     (* for testing/debugging *)
open Lacaml.Io;;  (* for testing/debugging *)
*)

(* open Qlib.States;; *)

open Lacamlext;;
open Lacaml.Z;;

module H = Hashtbl;;


(**********************************************************************************
  *                                                                               *
  *                           General Utility Functions                           *
  *                                                                               *
  *********************************************************************************)

(*
  *  Converts numbers to a strings
  *)
let to_string = Int.to_string;;
let float_to_string x = Printf.sprintf "%.5f" x;;


(**
  *  Converts a command to a friendly, human-readable string.
  *)
let cmd_to_string c = (
  let state_to_str (c0, c1) = (
    "((" 
      ^ (Float.to_string c0.Complex.re) ^ ", " 
      ^ (Float.to_string c0.im) ^ "i), (" 
      ^ (Float.to_string c1.Complex.re) ^ ", " 
      ^ (Float.to_string c1.im) ^ "i))"
  ) in
  let parse_input input = (
    match input with
    | Zero            -> "Zero"
    | One             -> "One"
    | Plus            -> "Plus"
    | Minus           -> "Minus"
    | State(c0, c1)   -> state_to_str (c0, c1)
  ) in
  let parse_basis basis = (
    match basis with
    | X -> "+/-"
    | Y -> "i/-i"
    | Z -> "0/1"
    | Comp -> "0/1"
    | FromTuples(a, b) -> (state_to_str a) ^ "/" ^ (state_to_str b)
    | FromAngle(a) -> "Angle(" ^ (Float.to_string a) ^ ")"
  ) in
  match c with 
  | Prep (qubit)          -> (
    "Prep(" ^ (to_string qubit) ^ ")"
  )
  | Input (qubit, input)  -> (
    let value = parse_input input in
    "Input(" ^ (to_string qubit) ^ ", " ^ value ^ ")"
  )
  | PrepList (qubits)     -> (
    "PrepList(" ^ (String.concat ", " (List.map to_string qubits)) ^ ")"
  )
  | InputList (args)    -> (
    let helper (q, i) = "(" ^ (to_string q) ^ ", " ^ (parse_input i) ^ ")" in
    "InputList(" ^ (String.concat ", " (List.map helper args)) ^ ")"
  )
  | Entangle (left, right)  -> (
    "E(" ^ to_string left ^ ", " ^ to_string right ^ ")"
  )
  | Measure (qubit, angle, parity1, parity2) -> (
    "M(" ^ String.concat ", " [to_string qubit; 
                              float_to_string angle; 
                              "[" ^ String.concat ", " (List.map to_string parity1) ^ "]"; 
                              "[" ^ String.concat ", " (List.map to_string parity2) ^ "]"] ^ ")"
  )
  | XCorrect (qubit, signals) -> (
    "X(" ^ to_string qubit ^ ", [" ^ String.concat ", " (List.map to_string signals) ^ "])"
  )
  | ZCorrect (qubit, signals) -> (
    "Z(" ^ to_string qubit ^ ", [" ^ String.concat ", " (List.map to_string signals) ^ "])"
  )
  | ReadOut (qubit, basis) -> (
    "ReadOut(" ^ to_string qubit ^ ", " ^ parse_basis basis ^ ")"
  )
);;


(**
  *  Prints out the program in a friendly, human-readable format.
  *)
let print_prog (cmds : prog) = (
  List.iter (fun x -> print_endline(cmd_to_string(x))) cmds
);;


(**
  *  Prints an error messgae to stdout noting the command and violation.
  *)
let print_err (cmd, msg) = (
  if (Option.is_none cmd) then (
    print_endline ("Error: " ^ msg ^ ".")
  ) else (
    print_endline ("Error: " ^ cmd_to_string (Option.get cmd) ^ " : " ^ msg ^ ".")
  )
);;

(**
  * Parses a program to determine the number of qubits being used.
  *)
let calc_qubit_num p = (
  let qubit_tbl = H.create 4 in
  let insert q = (
    if H.find_opt qubit_tbl q == None then (
      H.add qubit_tbl q ()
    ) else ()
  ) in
  let rec helper c = (
    match c with 
    | Prep (qubit)        -> insert qubit
    | Input (qubit, _)    -> insert qubit
    | PrepList (qubits)   -> (
      List.iter (fun x -> (helper (Prep(x)))) qubits
    )
    | InputList (qubits)  -> (
      List.iter (fun (q, i) -> (helper (Input(q, i)))) qubits
    )
    | Entangle (left, right)    -> (insert left; insert right)
    | Measure (qubit, _, _, _)  -> insert qubit
    | XCorrect (qubit, _)       -> insert qubit
    | ZCorrect (qubit, _)       -> insert qubit
    | ReadOut (qubit, _)        -> insert qubit
  ) in (
    List.iter helper p;
    H.length qubit_tbl
  )
);;


(**
  * Utility used for `well_formed` below.
  *
  * D2 (see below) is violated if any command acts on an unprepared, non-input qubit.
  * `comp_space_tbl` contains all qubits, prepared or input; `in_tbl` contains all
  * input qubits; thus, an error has occured if a command acts on a qubit not in 
  * `input_or_prepared_tbl`.
  *
  * Also checks that the ReadOut bases are orthogonal.
  *
  * Returns nothing. May have side-effects on `err`.
  *)
let check_D2 (err, prep_tbl, in_tbl) c = (
  let insert_input q = (
    if H.mem in_tbl q then (
      err := true;
      print_err (Some c, "Attempting to declare as input an already declared input qubit " ^ to_string q)
    ) else if H.mem prep_tbl q then (
      err := true;
      print_err (Some c, "Attempting to declare as input an already prepared qubit " ^ to_string q)
    ) else (
      H.add in_tbl q ()
    )
  ) in
  let insert_prepared q = (
    if H.mem in_tbl q then (
      err := true;
      print_err (Some c, "Attempting to prepare an already declared input qubit " ^ to_string q)
    ) else if H.mem prep_tbl q then (
      err := true;
      print_err (Some c, "Attempting to prepare an already prepared qubit " ^ to_string q)
    ) else (
      H.add prep_tbl q ()
    )
  ) in
  let check_qubit q = (
    if not (H.mem prep_tbl q || H.mem in_tbl q) then (
      err := true;
      print_err (Some c, "Invalid use of an unprepared, non-input qubit " ^ to_string q)
    )
  ) in
  let check_basis b = (
    match b with
    | FromTuples(t1, t2) -> (
      let (b1, b2) = Qlib.Bases.from_tuples t1 t2 in
      let res = gemm ~transa:`C b1 b2 in
      let res = (Mat.to_array res).(0).(0) in
      if res.re != 0.0 || res.im != 0.0 then (
        err := true;
        print_err (Some c, "Invalid read-out basis vectors must be orthogonal.")
      )
    )
    | _ -> ()
  )
  in (
    match c with 
    | Prep (qubit)        -> insert_prepared qubit
    | Input (qubit, _)    -> insert_input qubit
    | PrepList (qubits)   -> (
      List.iter (fun x -> (insert_prepared x)) qubits
    )
    | InputList (qubits)  -> (
      List.iter (fun (q, _) -> (insert_input q)) qubits
    )
    | Entangle (left, right)    -> (check_qubit left; check_qubit right)
    | Measure (qubit, _, _, _)  -> check_qubit qubit
    | XCorrect (qubit, _)       -> check_qubit qubit
    | ZCorrect (qubit, _)       -> check_qubit qubit
    | ReadOut (qubit, basis)    -> (check_qubit qubit; check_basis basis)
  )
);;

(**
  *  Utility used for `well_formed` below.
  *
  *  D1 (see below) is violated if any command acts on a measured qubit.
  *  Constructs a table containing measured qubits in the process.
  *
  *  Returns nothing. May have side-effects on `err`.
  *)
let check_D1 (err, meas_tbl) c = (
  let check q = (
    if (H.mem meas_tbl q) then (
      err := true;
      print_err (Some c, "Invalid use of already measured qubit " ^ to_string q)
    )
  ) in
  let insert q = (
    H.add meas_tbl q ()
  )
  in (
    match c with 
    | Prep (qubit)        -> check qubit
    | Input (qubit, _)    -> check qubit
    | PrepList (qubits)   -> (
      List.iter (fun x -> (check x)) qubits
    )
    | InputList (qubits)  -> (
      List.iter (fun (q, _) -> (check q)) qubits
    )
    | Entangle (left, right)    -> (check left; check right)
    | Measure (qubit, _, _, _)  -> insert qubit
    | XCorrect (qubit, _)       -> check qubit
    | ZCorrect (qubit, _)       -> check qubit
    | ReadOut (qubit, _)        -> check qubit
  )
);;

(**
  *  Utility used for `well_formed` below.
  *
  *  D0 (see below) is violated if any command depends on the outcome of an unmeasured qubit.
  *
  *  Returns nothing. May have side-effects on `err`.
  *)
let check_D0 (err, meas_tbl) c = (
  let check q = (
    if not (H.mem meas_tbl q) then (
      err := true;
      print_err (Some c, "Command depends on the outcome of unmeasured qubit " ^ to_string q)
    )
  ) 
  in (
    match c with 
    | Measure (_, _, signals1, signals2)  -> (
      List.iter check signals1;
      List.iter check signals2
    )
    | XCorrect (_, signals) -> (List.iter check signals)
    | ZCorrect (_, signals) -> (List.iter check signals)
    | _ -> ()
  )
);;

(**
  *  Utility used for `well_formed` below.
  *
  *  D4 (see below) is violated if the set of qubit integers does not equal the set of
  *  all integers between 0 and n-1 where n is the number of qubits used.
  *
  *  Returns nothing. May have side-effects on `err`.
  *)
let check_D4 (err, prep_tbl, in_tbl) = (
  let l = (H.length prep_tbl) + (H.length in_tbl) in
  let correct_keys = List.init l (fun x -> x) in
  let check ms k = if not (H.mem prep_tbl k || H.mem in_tbl k) then k::ms else ms in
  let missing = List.fold_left check [] correct_keys in (
    if (List.length missing > 0) then (
      err := true;
      print_err (None, "Invalid Program : expected qubits to be integers 0 through " 
                        ^ (to_string (l-1)) ^ "; missing " 
                        ^ (String.concat ", " (List.map to_string missing)))
    ) 
  )
);;


(**
  *  Checks whether a program is well formed, i.e., whether it satisfies
  *  the following conditions:
  *    From the paper:
  *      (D0) No command depends on an outcome not yet measured. (check signal qubits)
  *      (D1) No command acts on a qubit already measured.
  *      (D2) No command acts on a qubit not yet prepared, unless it is an input qubit.
  *      (D3) A qubit i is measured if and only if i is not an output.
  *           Equivalently, a qubit i is an output iff i is not measured. 
  *    Additional constraints per the current implementation:
  *      (D4) The "qubit integers" must start at 0 and increase without skipping an integer.
  *
  *  Returns true if the program is valid, false otherwise.
  *)
let well_formed (cmds : prog) : bool = (
  let err = ref false in
  let prep_tbl = H.create 12 in
  let in_tbl = H.create 12 in
  (* let out_tbl = H.create 12 in *) 
  let meas_tbl = H.create 12 in (
    List.iter (check_D2 (err, prep_tbl, in_tbl)) cmds;
    (*  At this point in the computation, prep_tbl contains all prepared qubits
        and in_tbl contains all input qubits *)
    List.iter (check_D1 (err, meas_tbl)) cmds;
    List.iter (check_D0 (err, meas_tbl)) cmds;
    check_D4 (err, prep_tbl, in_tbl)
  );
  if !err then false else true
);;


let rec parse_pattern pattern = (
  let helper p = (
    match p with
    | J (angle, q1, q2) -> [
      Entangle(q1, q2);
      Measure(q1, -.angle, [], []);
      XCorrect(q2, [q1]);
    ]
    | CZ (q1, q2) -> [
      Entangle(q1, q2);
    ]
    | H (q1, q2) -> parse_pattern [J(0.0, q1, q2)]
    | CX (q1, q2, q3, q4) -> parse_pattern [H(q2, q3); CZ(q1, q3); H(q3, q4)]
    | CNOT (q1, q2, q3, q4) -> parse_pattern [CX(q1, q2, q3, q4)]
    | RX (angle, q1, q2, q3) -> parse_pattern [H(q1, q2); J(angle, q2, q3)]
    | RZ (angle, q1, q2, q3) -> parse_pattern [J(angle, q1, q2); H(q2, q3)]
    | P (angle, q1, q2, q3) -> parse_pattern [RZ(angle, q1, q2, q3)]
    | CP (angle, q1, q2, q3, q4, q5, q6, q7, q8, q9, q10) -> (
      let angle' = Float.div angle 2. in parse_pattern [
        P(angle', q2, q3, q10);
        CNOT(q1, q4, q5, q10);
        P(-.angle', q6, q7, q10);
        CNOT(q1, q8, q9, q10)
      ]
    )
    | CP2 (angle, q1, q2, q3, q4, q5, qx) -> (
      let angle' = Float.div angle 2. in parse_pattern [
        J(angle', q2, qx);
        CZ(q1, qx);
        H(q3, qx);
        J(-.angle', q4, qx);
        CZ(q1, qx);
        H(q5, qx)
      ]
    )
    | CMD (cmd) -> [cmd]
  ) in
  List.fold_left (fun ls p -> ls @ (helper p)) [] pattern
);;

let print_pattern pattern = (
  let itos = Int.to_string in
  let ftos = Float.to_string in
  let helper p = (
    match p with
    | J (angle, q1, q2) -> (
      "J(" ^ (String.concat ", " [ftos angle; itos q1; itos q2]) ^ ")"
    )
    | CZ (q1, q2) -> (
      "^Z(" ^ (String.concat ", " [itos q1; itos q2]) ^ ")"
    )
    | H (q1, q2) -> (
      "H(" ^ (String.concat ", " [itos q1; itos q2]) ^ ")"
    )
    | CX (q1, q2, q3, q4) -> (
      "^X(" ^ (String.concat ", " (List.map itos [q1; q2; q3; q4])) ^ ")"
    )
    | CNOT (q1, q2, q3, q4) -> (
      "CNOT(" ^ (String.concat ", " (List.map itos [q1; q2; q3; q4])) ^ ")"
    )
    | RX (angle, q1, q2, q3) -> (
      "Rx(" ^ (String.concat ", " ((ftos angle)::(List.map itos [q1; q2; q3]))) ^ ")"
    )
    | RZ (angle, q1, q2, q3) -> (
      "Rz(" ^ (String.concat ", " ((ftos angle)::(List.map itos [q1; q2; q3]))) ^ ")"
    )
    | P (angle, q1, q2, q3) -> (
      "P(" ^ (String.concat ", " ((ftos angle)::(List.map itos [q1; q2; q3]))) ^ ")"
    )
    | CP (angle, q1, q2, q3, q4, q5, q6, q7, q8, q9, q10) -> (
      "^P(" ^ (String.concat ", " ((ftos angle)::(List.map itos [q1; q2; q3; q4; q5; q6; q7; q8; q9; q10]))) ^ ")"
    )
    | CP2 (angle, q1, q2, q3, q4, q5, qx) -> (
      "^P(" ^ (String.concat ", " ((ftos angle)::(List.map itos [q1; q2; q3; q4; q5; qx]))) ^ ")"
    )
    | CMD(cmd) -> (
      "CMD(" ^ (cmd_to_string cmd) ^ ")"
    )
  ) in
  List.iter (fun p -> print_endline (helper p)) pattern
);;


(**
  *  Constructs a table containing the output qubits by appealing to D3 above.
  *
  *  Returns the constructed table.
  *)
let get_output_qubits cmds = (
  let meas_tbl = H.create 12 in
  let out_tbl = H.create 12 in
  let insert_meas q = (
    H.add meas_tbl q ()
  ) in
  let insert_out q = (
    if not (H.mem meas_tbl q) then (
      H.add out_tbl q ()
    )
  ) in 
  let meas_helper c = (
    match c with
    | Measure (qubit, _, _, _)  -> insert_meas qubit
    | _ -> ()
  ) in
  let out_helper c = (
    match c with
    | Prep (qubit)        -> insert_out qubit
    | Input (qubit, _)    -> insert_out qubit
    | PrepList (qubits)   -> (
      List.iter (fun q -> insert_out q) qubits
    )
    | InputList (qubits)  -> (
      List.iter (fun (q, _) -> insert_out q) qubits
    )
    | _ -> ()
  ) in (
    List.iter meas_helper cmds;
    List.iter out_helper cmds;
    out_tbl
  )
);;


(**
  * Standardizes the input program by applying the following rewriting rules to the program:
  *   (1) X^s_i E_{ij} => E_{ij} Z^s_j X^s_i
  *   (2) X^s_j E_{ij} => E_{ij} Z^s_i X^s_j
  *   (3) Z^s_i E_{ij} => E_{ij} Z^s_i 
  *   (4) Z^s_j E_{ij} => E_{ij} Z^s_j
  *   (5) X^r_i ^t[M^{\alpha}_i]^s => ^t[M^{\alpha}_i]^{s+r}
  *   (6) Z^r_i ^t[M^{\alpha}_i]^s => ^{r+t}[M^{\alpha}_i]^s
  * The following three rules work on disjoin sets of qubits:
  *   (7) A_k E_{ij} => E_{ij} A_k  where A is not an entanglement
  *   (8) X^s_i A_k  => A_k X^s_i   where A is not a correction
  *   (9) Z^s_i A_k  => A_k Z^s_i   where A is not a correction
  * Grammar specific:
  *   (10) Any prep and input commands may be moved directly to the front in any arbitrary order.
  *
  * Returns the standardized program
  *)
let standardize prog = (
  (* Extracts prep and input commands into their own list. *)
  let extract_prep cmd (prep_cmds, main_cmds) = (
    let helper c = (c::prep_cmds, main_cmds) in
    match cmd with
    | Prep (_)      -> helper cmd
    | Input (_)     -> helper cmd
    | PrepList (_)  -> helper cmd
    | InputList (_) -> helper cmd
    | _ -> (prep_cmds, cmd::main_cmds)
  ) in
  let extract_readout cmd (readout_cmds, main_cmds) = (
    let helper c = (c::readout_cmds, main_cmds) in
    match cmd with
    | ReadOut (_)      -> helper cmd
    | _ -> (readout_cmds, cmd::main_cmds)
  ) in
  let prep_cmds, temp_cmds = List.fold_right extract_prep prog ([], [])  in
  let readout_cmds, main_cmds = List.fold_right extract_readout temp_cmds ([], [])  in

  (* Performs rewriting rules on the main commands until unable to do so. *)
  let rec rewrite stable p = (
    let rewrite = rewrite stable in
    match p with
    | a::b::cmds -> (
      match (a, b) with
      | (XCorrect(q, s), Entangle(i, j)) -> (
        stable := false;
        if (q == i) then (
          [Entangle(i, j); ZCorrect(j, s)] @ (rewrite (XCorrect(i, s)::cmds))
        ) else if (q == j) then (
          [Entangle(i, j); ZCorrect(i, s)] @ (rewrite (XCorrect(j, s)::cmds))
        ) else (
          [b] @ (rewrite (a::cmds))
        )
      )
      | (ZCorrect(q, s), Entangle(i, j)) -> (
        stable := false;
        if (q == i) then (
          [Entangle(i, j)] @ (rewrite (ZCorrect(i, s)::cmds))
        ) else if (q == j) then (
          [Entangle(i, j)] @ (rewrite (ZCorrect(j, s)::cmds))
        ) else (
          [b] @ (rewrite (a::cmds))
        )
      )
      | (XCorrect(q_x, sig_r), Measure(q_m, angle, sig_s, sig_t)) -> (
        stable := false;
        if (q_x == q_m) then (
          rewrite (Measure(q_m, angle, sig_s @ sig_r, sig_t)::cmds)
        ) else (
          [b] @ (rewrite (a::cmds))
        )
      )
      | (ZCorrect(q_z, sig_r), Measure(q_m, angle, sig_s, sig_t)) -> (
        stable := false;
        if (q_z == q_m) then (
          rewrite (Measure(q_m, angle, sig_s, sig_r @ sig_t)::cmds)
        ) else (
          [b] @ (rewrite (a::cmds))
        )
      )
      | (Measure(q, _, _, _), Entangle(i, j)) -> (
        stable := false;
        if ((q == i) || (q == j)) then (
          [a] @ (rewrite (b::cmds))
        ) else (
          [b] @ (rewrite (a::cmds))
        )
      )
      | _ -> [a] @ (rewrite (b::cmds))
    )
    | [a] -> a::rewrite([])
    | []  -> []
  ) in
  let stable = ref true in
  let rec helper cmds = (
    let temp = rewrite stable cmds in
    if !stable then temp else (
      stable := true;
      helper temp
    )
  ) in
  let main_cmds' = helper main_cmds in

  (* Returns the full program. *)
  prep_cmds @ main_cmds' @ readout_cmds
);;

(** Extracts the prep and input commands from the front of a
  * standardized program, expands any "List" commands, and
  * orders the commands based on qubit number 
  *)
let expand_and_order_prep prog = (
  let rec extract_prep cmds = (
    let helper c cmds' = (
      let (a, b) = extract_prep cmds' in
      (c::a, b)
    ) in
    match cmds with
    | c::cmds' -> (
      match c with
      | Prep (_)      -> helper c cmds'
      | Input (_)     -> helper c cmds'
      | PrepList (_)  -> helper c cmds'
      | InputList (_) -> helper c cmds'
      | _ -> ([], c::cmds')
    )
    | [] -> ([], [])
  ) in
  let prep_cmds, main_cmds = extract_prep prog in

  let expand_prep c cmds = (
    match c with
      | Prep (_)        -> c::cmds 
      | Input (_)       -> c::cmds
      | PrepList (qs)   -> (List.map (fun q -> Prep(q)) qs) @ cmds
      | InputList (qs)  -> (List.map (fun (q, i) -> Input(q, i)) qs) @ cmds 
      | _ -> cmds
  ) in
  let expanded_prep_cmds = List.fold_right expand_prep prep_cmds [] in

  let order_prep cmds = (
    let cmp x y = (
      let get_q c = (
        match c with
        | Prep(q)     -> q
        | Input(q, _) -> q
        | _ -> -1
      ) in
      if (get_q x) < (get_q y) then -1 else 1
    ) in
    List.sort cmp cmds
  ) in
  (order_prep expanded_prep_cmds) @ main_cmds
);;

let performance func p = (
  let t = Sys.time() in
  let _ = func p in
  Float.sub (Sys.time()) t
);;

let unpack_qtbl qtbl = (
  let ls = List.of_seq (Hashtbl.to_seq qtbl) in
  let compare (_, p1) (_, p2) = p1 - p2 in
  let sorted = List.sort compare ls in
  let (qs, _) = (
    List.fold_right (fun (q, p) (qs, ps) -> (q::qs, p::ps)) sorted ([], [])
  ) in qs
);;

let print_qubits qs = (
  print_endline (String.concat ", " (List.map Int.to_string qs))
);;
