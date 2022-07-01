
open Base;;
open Python_lib;;
open Python_lib.Let_syntax;;
open Backend.Types;;

type serialized_cmd = 
  {
    cmd_type: string;
  }
[@@deriving python]
;;


let approx_pi = 
  let%map_open n = positional "n" int ~docstring:"the value n" in
    let sum = 
      List.init n ~f:(fun i ->
        let i = Float.of_int(1 + i) in
        1.0 /. (i *. i))
        |> List.reduce_exn ~f:( +. )
      in
      Float.sqrt (sum *. 6.) |> python_of_float
    ;;

let print_from_ocaml = 
  let%map_open str = keyword "string" string ~docstring:"the string to print" in 
    let new_str = "This is from OCaml: " ^ str in
      python_of_string new_str
;;

(*
  Helpers
*)
let rec custom_map f = function 
  [] -> []
  | head :: tail -> 
    let r = f head in 
    r :: custom_map f tail;;

let append_item lst a = lst @ [a]

let rec insert_at_end l i =
  match l with
    [] -> [i]
  | h :: t -> h :: (insert_at_end t i)


(* let convert_to_json strings = 
  List.iter ~f:Stdio.print_endline strings
;; *) 

let create_Yojson_list_from_int_list list : Yojson.Basic.t list = (
  let f element = (
    `Int element
  ) in (
    List.map ~f:f list 
  )
)

let build_json_from_program program = (
  let find_match command : Yojson.Basic.t = (
    match command with 
    | Prep (qubit) ->
        `Assoc [
          ("Prep", `Assoc [
            ("on_qubits", `List [`Int qubit;])
          ])
        ]
    | Entangle (left, right) ->
        `Assoc [
          ("Entangle", `Assoc [
            ("on_qubits", `List [`Int left; `Int right;])
          ])
        ]
    | Measure (qubit, angle, signal_s, signal_t) ->
        `Assoc [
          ("Measure", `Assoc [
            ("on_qubits", `List [`Int qubit;]);
            ("angle", `Float angle);
            ("signal_s", `List (create_Yojson_list_from_int_list signal_s));
            ("signal_t", `List (create_Yojson_list_from_int_list signal_t))
          ])
        ]
    | XCorrect (qubit, signals) ->
        `Assoc [
          ("XCorrect", `Assoc [
            ("on_qubits", `List [`Int qubit;]);
            ("signals", `List (create_Yojson_list_from_int_list signals))
          ])
        ]
    | ZCorrect (qubit, signals) ->
        `Assoc [
          ("ZCorrect", `Assoc [
            ("on_qubits", `List [`Int qubit;]);
            ("signals", `List (create_Yojson_list_from_int_list signals))
          ])
        ]
    | _ ->
      `String "Unimplemented Command"
  ) in (
    List.map ~f:find_match program
  )
);;

(* Minimal Example, quantum teleportation from paper *)
let program = [
  Prep(1); 
  Prep(2); 
  Prep(3); 
  Entangle(1, 2);
  Measure(1, 0.0, [], []);
  XCorrect(2, [1]);
  Entangle(2, 3);
  Measure(2, 0.0, [], []);
  XCorrect(3, [2]);
]
(* Standardarized quantum teleportation from paper *)
let program2 = [
  Prep(1);
  Prep(2);
  Prep(3);
  Entangle(1, 2);
  Entangle(2, 3);
  Measure(1, 0.0, [], []);
  Measure(2, 0.0, [], []);
  ZCorrect(3, [1]);
  XCorrect(3, [2])
]

let x_rotation_2pi = [
  Prep(1);
  Prep(2);
  Prep(3);
  Entangle(1, 2);
  Entangle(2, 3);
  Measure(1, 0.0, [], []);
  Measure(2, -6.28318, [1], []);
  ZCorrect(3, [1]);
  XCorrect(3, [2]);
]

let z_rotation_2pi = [
  Prep(1);
  Prep(2);
  Prep(3);
  Entangle(1, 2);
  Entangle(2, 3);
  Measure(1, -6.28318, [], []);
  Measure(2, 0.0, [], []);
  ZCorrect(3, [1]);
  XCorrect(3, [2]);
]

let grovers = [
  Prep(0);
  Prep(1);
  Prep(2);
  Prep(3);
  Entangle(2, 3);
  Entangle(2, 0);
  Entangle(3, 1);
  Entangle(0, 1);
  Measure(2, 0.0, [], []);
  Measure(3, 0.0, [], []);
  Measure(0, -3.14159, [2], [3]);
  Measure(1, -3.14159, [3], [2]);
]

let () =
  let res = build_json_from_program program2 in
      let serialized = `List res in 
        Yojson.Basic.pretty_to_channel Stdio.stdout serialized;;
;;


(* if not (Py.is_initialized ()) then Py.initialize ();

let mod_ = Py_module.create "mcl" in
(* General test fns *)
Py_module.set mod_ "approx_pi" approx_pi;
Py_module.set mod_ "print_from_ocaml" print_from_ocaml;
Py_module.set mod_ "convert_program_to_string" conver t_program_to_string; *)