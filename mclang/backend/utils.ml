
open Types;;

(*
open Format;;     (* for testing/debugging *)
open Lacaml.Io;;  (* for testing/debugging *)
*)

(* open Qlib.States;; *)

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
  let parse_input input = (
    match input with
    | Zero            -> "Zero"
    | One             -> "One"
    | Plus            -> "Plus"
    | Minus           -> "Minus"
    | State(c0, c1)   -> "((" 
                          ^ (Float.to_string c0.re) ^ ", " 
                          ^ (Float.to_string c0.im) ^ "i), (" 
                          ^ (Float.to_string c1.re) ^ ", " 
                          ^ (Float.to_string c1.im) ^ "i))"
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
  let check q = (
    if not (H.mem prep_tbl q || H.mem in_tbl q) then (
      err := true;
      print_err (Some c, "Invalid use of an unprepared, non-input qubit " ^ to_string q)
    )
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
    | Entangle (left, right)    -> (check left; check right)
    | Measure (qubit, _, _, _)  -> check qubit
    | XCorrect (qubit, _)       -> check qubit
    | ZCorrect (qubit, _)       -> check qubit
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
    | Measure (qubit, _, _, _)  -> (insert qubit)
    | XCorrect (qubit, _)       -> (check qubit)
    | ZCorrect (qubit, _)       -> (check qubit)
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
  *  Utility used for `well_formed` below.
  *
  *  Constructs a table containing the output qubits by appealing to D3
  *
  *  Returns nothing.
  *)(*
let construct_output_tbl (out_tbl, meas_tbl) c = (
  let handler q = (
    if not (H.mem meas_tbl q) then (
      H.add out_tbl q ()
    )
  )
  in (
    match c with
    | Entangle (left, right)  -> (handler left; handler right)
    | Measure (_, _, _, _)    -> ()
    | XCorrect (qubit, _)     -> (handler qubit)
    | ZCorrect (qubit, _)     -> (handler qubit)
    | _ -> ()
  )
);;
*)

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
    (* List.iter (construct_output_tbl (out_tbl, meas_tbl)) cmds (* not needed for well_formed *) *)
  );
  if !err then false else true
);;


let parse_pattern pattern = (
  let helper p = (
    match p with
    | J (angle, q1, q2) -> [
      Entangle(q1, q2);
      Measure(q1, -.angle, [], []);
      XCorrect(q2, [q1]);
    ]
    | Z (q1, q2) -> [
      Entangle(q1, q2);
    ]
  ) in
  List.fold_left (fun ls p -> ls @ (helper p)) [] pattern
);;
