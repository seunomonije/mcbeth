
open Base;;
open Python_lib;;
open Python_lib.Let_syntax;;

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

(*
*
* Start minimal example
*
*)
let simple_program : Yojson.Basic.t list = [
  `Assoc 
    [("Prep", `Int 0)];
  `Assoc
    [("Prep", `Int 1)];
  `Assoc
    [("Prep", `Int 2)];
];;

let serialized_program = `List simple_program

let () =
  Yojson.Basic.pretty_to_channel Stdio.stdout serialized_program;;
;;


(* if not (Py.is_initialized ()) then Py.initialize ();

let mod_ = Py_module.create "mcl" in
(* General test fns *)
Py_module.set mod_ "approx_pi" approx_pi;
Py_module.set mod_ "print_from_ocaml" print_from_ocaml;
Py_module.set mod_ "convert_program_to_string" conver t_program_to_string; *)