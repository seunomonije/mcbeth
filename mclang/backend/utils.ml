
open Types;;

open Lacamlext;;
open Lacaml.Z;;

open Format;;     (* for testing/debugging *)
open Lacaml.Io;;  (* for testing/debugging *)

open Qlib.States;;

module H = Hashtbl;;


(**********************************************************************************
  *                                                                               *
  *                               Utility Functions                               *
  *                                                                               *
  *********************************************************************************)

(*
  *  Converts numbers to a strings
  *)
let to_string = Int.to_string;;
let float_to_string x = Printf.sprintf "%.5f" x;;

(**
  *  Converts a preperation to a friendly, human-readable string.
  *)
let prep_to_string p = (
  match p with
  | Init (qubit, base_angle) -> 
    "Init[" ^ to_string qubit ^ ", " ^ float_to_string base_angle ^ "]"
  | Init0 (qubit) -> 
    "Init0[" ^ to_string qubit ^ "]"
  | Init1 (qubit) ->
    "Init1[" ^ to_string qubit ^ "]"
  | InitPlus (qubit) ->
    "Init+[" ^ to_string qubit ^ "]"
  | InitMinus (qubit) ->
    "Init-[" ^ to_string qubit ^ "]"
  | InitNonInput (qubits) ->
    "InitNonInput[" ^ (String.concat ", " (List.map to_string qubits)) ^ "]"
);;

(**
  *  Converts a command to a friendly, human-readable string.
  *)
let cmd_to_string c = (
  match c with 
  | Entangle (left, right) ->
    "E[" ^ to_string left ^ ", " ^ to_string right ^ "]"
  | Measure (qubit, angle, parity1, parity2) ->
    "M[" ^ String.concat ", " [to_string qubit; 
                              float_to_string angle; 
                              "[" ^ String.concat ", " (List.map to_string parity1) ^ "]"; 
                              "[" ^ String.concat ", " (List.map to_string parity2) ^ "]"] ^ "]"
  | XCorrect (qubit, signals) ->
    "X[" ^ to_string qubit ^ ", [" ^ String.concat ", " (List.map to_string signals) ^ "]]"
  | ZCorrect (qubit, signals) ->
    "Z[" ^ to_string qubit ^ ", [" ^ String.concat ", " (List.map to_string signals) ^ "]]"
);;


(**
  *  Prints out the program in a friendly, human-readable format.
  *)
let print_prog ((preps, cmds) : prog) = (
  List.iter (fun x -> print_endline(prep_to_string(x))) preps;
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
    | Init (qubit, _)   -> insert qubit
    | Init0 (qubit)     -> insert qubit
    | Init1 (qubit)     -> insert qubit
    | InitPlus (qubit)  -> insert qubit
    | InitMinus (qubit) -> insert qubit
    | InitNonInput (qubits) -> (
      List.iter (fun x -> (helper states (InitPlus(x)))) qubits
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
  *  Utility used for `well_formed` below.
  *
  *  Constructs tables for containing the input qubits 
  *  and another table containg all qubits in the computational space.
  *
  *  Returns nothing.
  *)
let check_prep (comp_space_tbl, in_tbl) p = (
  let insert q = (
    H.add comp_space_tbl q ();
    H.add in_tbl q ()
  ) 
  in (
    match p with
    | Init (qubit, _)   -> insert qubit
    | Init0 (qubit)     -> insert qubit
    | Init1 (qubit)     -> insert qubit
    | InitPlus (qubit)  -> insert qubit
    | InitMinus (qubit) -> insert qubit
    | InitNonInput (qubits) -> (
      List.iter (fun x -> H.add comp_space_tbl x ()) qubits
    )
  )
);;


(**
  *  Utility used for `well_formed` below.
  *
  *  D2 (see below) is violated if any command acts on an unprepared, non-input qubit.
  *  `input_or_prepared_tbl` contains all prepared or input qubits; thus, an error
  *  has occured if a command acts on a qubit not in `input_or_prepared_tbl`.
  *
  *  Returns nothing. May have side-effects on `err`.
  *)
let check_D2 (err, input_or_prepared_tbl) c = (
  let check q = (
    if not (H.mem input_or_prepared_tbl q) then (
      err := true;
      print_err (Some c, "Invalid use of unprepared, non-input qubit " ^ to_string q)
    )
  ) 
  in (
    match c with 
    | Entangle (left, right)    -> (check left; check right)
    | Measure (qubit, _, _, _)  -> (check qubit)
    | XCorrect (qubit, _)       -> (check qubit)
    | ZCorrect (qubit, _)       -> (check qubit)
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
    | Entangle (_, _) -> ()
    | Measure (_, _, signals1, signals2)  -> (
      List.iter check signals1;
      List.iter check signals2
    )
    | XCorrect (_, signals) -> (List.iter check signals)
    | ZCorrect (_, signals) -> (List.iter check signals)
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
let check_D4 (err, comp_space_tbl) = (
  let l = H.length comp_space_tbl in
  let correct_keys = List.init l (fun x -> x) in
  let check ms k = if not (H.mem comp_space_tbl k) then k::ms else ms in
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
    | Entangle (left, right)    -> (handler left; handler right)
    | Measure (_, _, _, _)  -> ()
    | XCorrect (qubit, _)       -> (handler qubit)
    | ZCorrect (qubit, _)       -> (handler qubit)
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
  *  Returns 0 on failure.
  *  On success, returns the number of qubits used in the program.
  *)
let well_formed ((preps, cmds) : prog) : int = (
  let err = ref false in
  let comp_space_tbl = H.create 12 in
  let in_tbl = H.create 12 in
  (* let out_tbl = H.create 12 in *) 
  let meas_tbl = H.create 12 in (
    List.iter (check_prep (comp_space_tbl, in_tbl)) preps;
    (*  At this point in the computation, comp_space_tbl contains all qubits used
        and in_tbl contains all input qubits *)
    List.iter (check_D2 (err, comp_space_tbl)) cmds;
    List.iter (check_D1 (err, meas_tbl)) cmds;
    List.iter (check_D0 (err, meas_tbl)) cmds;
    check_D4 (err, comp_space_tbl)
    (* List.iter (construct_output_tbl (out_tbl, meas_tbl)) cmds (* not needed for well_formed *) *)
  );
  if (!err) then 0 else H.length comp_space_tbl
);;


(**
  * Calculates signals -- i.e., a single number based on the outcomes of qubits.
  *
  * Signal s = \Sum_{i \in I}(s_i) where s_i = 0 if the measurement of qubit i
  * collapses the state to |+_\alpha> and s_i = 1 if the state collapses to
  * |-_\alpha>. The summation is performed in Z_2. 
  *
  * Input:
  *   get_outcome : a function which given a qubit, returns the outcome of the qubit,
  *                 should be 0 or 1; returns -1 on error.
  *   qs          : a list of qubits which the signal depends on
  * Output:
  *   Returns the signal, 0 or 1.
  *)
let calc_signal (get_outcome : qubit -> int) (qs : qubit list) = (
  (List.fold_left (fun s q -> s + get_outcome q) 0 qs) mod 2
);;


(**
  * Calculates the new angle of a measurement based on the original angle
  * and the outcomes of signals1 and signals2
  *
  * Input:
  *   get_outcome : a function to be passed to calc_signal; see calc_signal above
  *   angle       : the original angle \alpha of type float
  *   signals_s   : a list of qubits
  *   signals_t   : a list of qubits
  *
  * Output:
  *   Returns the new angle, of type float.
  *)
let new_angle get_outcome angle signals_s signals_t = (
  let signal qs = float_of_int (calc_signal get_outcome qs) in
  let ( + ) = Float.add in
  let ( * ) = Float.mul in
  (-1.)**(signal signals_s) * angle + (signal signals_t) * Float.pi
);;


(**
  * Prints qubits
  *)
let print_states (states : Vec.t array) = (
  printf "states = @[%a@]@\n@\n" pp_cmat (Mat.of_col_vecs states)
);;


(**
  *  Performs appropriate operations to initialize qubits.
  *  `states` array in memory changed as a side-effect.
  *)
let rec prep (states : Vec.t array) (p : prep) : unit = (
  match p with
  | Init (qubit, _) -> (
    (* Initalizes a qubit with base `basis` *)
    states.(qubit) <- dummy_state
  )
  | Init0 (qubit) -> (
    (* states[qubit] = column vector of [1, 0] *)
    states.(qubit) <- zero_state
  )
  | Init1 (qubit) -> (
    states.(qubit) <- one_state
  )
  | InitPlus (qubit) -> (
    states.(qubit) <- plus_state
  )
  | InitMinus (qubit) -> (
    states.(qubit) <- minus_state
  )
  | InitNonInput (qubits) -> (
    (* Non-input qubits are all initialized to |+> *)
    List.iter (fun x -> (prep states (InitPlus(x)))) qubits
  )
);;

(**
  * 
  *)
let prep_qubits qubit_num preps = (
  let states = Array.make qubit_num Vec.empty in (
    List.iter (fun p -> prep states p) preps;
    states
  )
);;

