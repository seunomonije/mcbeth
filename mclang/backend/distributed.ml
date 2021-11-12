
open Types;;
open Utils;;
open Run;;

open Lacamlext;;
open Lacaml.Z;;


(**********************************************************************************
  *                                                                               *
  *                              Utility Functions                                *
  *                                                                               *
  *********************************************************************************)


let print_dist dist = (
  let groups = List.of_seq (Hashtbl.to_seq_values dist) in
  List.iter (fun group -> (
    let qubits = List.of_seq (Hashtbl.to_seq group) in
    let compare (_, x) (_, y) = x - y in
    print_endline (
      String.concat ", " (List.map (
        fun (q, _) -> (
          Int.to_string q
        )
      ) (List.sort compare qubits))
    )
  )) groups
);;

let print_dist_struct dist_struct = (
  List.iter (fun g -> (
    let (_, cmds) = Hashtbl.find dist_struct g in (
      print_endline ("\nNode " ^ (Int.to_string g) ^ ":");
      print_prog cmds;
    )
  )) (List.init (Hashtbl.length dist_struct) (fun x -> x));
);;

let print_dist_prog (non_dist_cmds, dist_struct) = (
  print_endline "Initial Setup:";
  print_prog non_dist_cmds;
  print_dist_struct dist_struct;
);;

let print_dist_approx = print_dist_struct;;

(**
  * Constructs the map of the distributed network by processing
  * a list of lists of qubits and returns the associated structure
  * of type "dist_map".
  *)
let build_dist_map qubit_categories = (
  let main_tbl = Hashtbl.create (List.length qubit_categories) in
  let constructNode i qubits = (
    let node = Hashtbl.create (List.length qubits) in
    let sorted_qubits = List.sort Stdlib.compare qubits in
    let _ = (
      List.fold_left (fun j q -> Hashtbl.add node q j; j+1) 0 sorted_qubits
    ) in Hashtbl.add main_tbl i node; i + 1 
  ) in
  let _ = (
    List.fold_left constructNode 0 qubit_categories
  ) in main_tbl
);;

let build_group_map dist = (
  let group_map = Hashtbl.create (Hashtbl.length dist) in (
    Hashtbl.iter (fun group qubits -> (
      Hashtbl.iter (fun q _ -> Hashtbl.add group_map q group) qubits
    )) dist;
    group_map
  )
);;

let build_group_map' dist_struct = (
  let group_map = Hashtbl.create (Hashtbl.length dist_struct) in (
    Hashtbl.iter (fun group (qubits, _) -> (
      Hashtbl.iter (fun q _ -> Hashtbl.add group_map q group) qubits
    )) dist_struct;
    group_map
  )
);;

let split_program dist cmds = (

  let group_map = build_group_map dist in

  let dist_cmds_tbl = Hashtbl.create (Hashtbl.length dist) in
  let _ = (
    (* Intializes dist_cmds_tbl with empty lists of commands *)
    Hashtbl.iter (fun group _ -> Hashtbl.add dist_cmds_tbl group []) dist
  ) in
  let split_helper cmd nondist_cmds = (
    let nondist_helper c = c::nondist_cmds in
    let dist_helper c q = (
      let group = Hashtbl.find group_map q in
      let group_cmds = Hashtbl.find dist_cmds_tbl group in
      Hashtbl.replace dist_cmds_tbl group (c::group_cmds);
      nondist_cmds
    ) in
    match cmd with
    | Prep (_)      -> nondist_helper cmd
    | Input (_)     -> nondist_helper cmd
    | PrepList (_)  -> nondist_helper cmd
    | InputList (_) -> nondist_helper cmd
    | Entangle (_)  -> nondist_helper cmd

    | Measure (qubit, _, _, _)  -> dist_helper cmd qubit
    | XCorrect (qubit, _)       -> dist_helper cmd qubit
    | ZCorrect (qubit, _)       -> dist_helper cmd qubit
    | ReadOut (qubit, _)        -> dist_helper cmd qubit
  ) in
  let non_dist_cmds = List.fold_right split_helper cmds []  in
  (non_dist_cmds, dist_cmds_tbl)
);;

let build_node_loc node_list = (
  let _, ls = (
    List.fold_left (fun (i, ls) x -> (i+1, (i, x)::ls)) (0, []) node_list
  ) in
  Hashtbl.of_seq (List.to_seq ls)
);;

(**********************************************************************************
  *                                                                               *
  *                              Build Distributed Programs                       *
  *                                                                               *
  *********************************************************************************)


let build_dist_prog dist cmds = (
  let (non_dist_cmds, dist_cmds_tbl) = split_program dist (standardize cmds) in
  let dist_struct = Hashtbl.create (Hashtbl.length dist) in (
    Hashtbl.iter (fun g qtbl -> (
      let cmds = Hashtbl.find dist_cmds_tbl g in
      Hashtbl.add dist_struct g (qtbl, cmds)
    )) dist;
    (non_dist_cmds, dist_struct)
  )
)

let approx_subsystems (non_dist_cmds, dist_struct) = (
  let dist_struct' = Hashtbl.copy dist_struct in
  let group_map = build_group_map' dist_struct' in
  let getGroup q = Hashtbl.find group_map q in (  
    List.iter (fun cmd -> (
      match cmd with
      | Prep (q) -> (
        let group = getGroup q in
        let (qtbl, prog) = Hashtbl.find dist_struct' group in
        Hashtbl.replace dist_struct' group (qtbl, cmd::prog)
      )
      | Input (q, _) -> (
        let group = getGroup q in
        let (qtbl, prog) = Hashtbl.find dist_struct' group in
        Hashtbl.replace dist_struct' group (qtbl, cmd::prog)
      )
      | Entangle (q1, q2) -> (
        let group1 = getGroup q1 in
        let group2 = getGroup q2 in
        if (group1 == group2) then (
          let (qtbl, prog) = Hashtbl.find dist_struct' group1 in
          Hashtbl.replace dist_struct' group1 (qtbl, cmd::prog)
        )
      )
      | _ -> ()
    )) (List.rev non_dist_cmds);
    dist_struct'
  )
)

(**
  * Calculates appoximate subsystems by sorting non_dist_cmds into
  * appropriate subsystems -- discards entanglement operations between
  * subsystems.
  *)
let build_dist_approx ((non_dist_cmds, dist_struct) : dist_prog) : dist_approx = (
  (* Prepares a hashtable mapping qubits to initial states *)
  let non_dist_cmds = expand_and_order_prep non_dist_cmds in
  let approx_dist_struct = approx_subsystems (non_dist_cmds, dist_struct) in
  approx_dist_struct
);;


(**********************************************************************************
  *                                                                               *
  *                              Run Distributed Programs                         *
  *                                                                               *
  *********************************************************************************)

let check_mtbl mtbl mtbl_lock q = (
  Mutex.lock mtbl_lock;
  if Hashtbl.mem mtbl q then true else (
    Mutex.unlock mtbl_lock;
    false
  )
)

let cmd_exec mtbl mtbl_lock qtbl statevec cmd = (
  (*let getPos x = Hashtbl.find qtbl x in
  let qubit_num = (Hashtbl.length qtbl) in*)
  let exec () = (
    rand_eval_cmd_exec ~mtbl_lock:(Some(mtbl_lock)) mtbl qtbl statevec cmd
  ) in
  let obtainDep signals = (
    List.iter (fun q -> (
      let check () = not (check_mtbl mtbl mtbl_lock q) in
      while check () do Thread.yield () done;
      Mutex.unlock mtbl_lock;
    )) signals
  ) in
  match cmd with
  | Measure(_, _, signals_s, signals_t) -> (
    obtainDep signals_s;
    obtainDep signals_t;
    exec ();
  )
  | XCorrect(_, signals) -> (
    obtainDep signals;
    exec ();
  ) 
  | ZCorrect(_, signals) -> (
    obtainDep signals;
    exec ();
  )
  | _ -> exec ();
);;

let run_subsystem out out_lock mtbl mtbl_lock (qtbl, cmds) group = (

  (* Step 1: execute each command in the program *)
  let qtbl' = Hashtbl.copy qtbl in
  let init_statevec = Mat.make 1 1 Complex.one in
  let cmd_exec' = cmd_exec mtbl mtbl_lock qtbl' in
  let statevec = Mat.cleanup (
    List.fold_left (fun sv c -> (cmd_exec' sv c)) init_statevec cmds
  ) in
  
  (* Step 2: add information to "out" *)
  let final_subsystem = (qtbl', statevec) in (
    Mutex.lock out_lock;
    Hashtbl.add out group final_subsystem;
    Mutex.unlock out_lock;
  )

);;

let run_dist_approx ?(print=false) dist_approx = (
  let out = Hashtbl.create (Hashtbl.length dist_approx) in
  let out_lock = Mutex.create () in
  let mtbl = Hashtbl.create 1 in
  let mtbl_lock = Mutex.create () in
  let threads = ref [] in (
    Hashtbl.iter (fun group sub_prog -> (
      (* spawns threads that run each subsystem *)
      let thread_func () = run_subsystem out out_lock mtbl mtbl_lock sub_prog group in
      let x = Thread.create thread_func () in
      threads := x::(!threads) 
    )) dist_approx;
    List.iter (fun h -> Thread.join h) !threads;
    if print then (
      Hashtbl.iter (fun group (qtbl, statevec) -> (
        print_endline ("\nNode " ^ (Int.to_string group) ^ ":");
        let qs = unpack_qtbl qtbl in
        print_qubits qs;
        print_endline "";
        Mat.print statevec
      )) out
    );
    (mtbl, out)
  )
);;
