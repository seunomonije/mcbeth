
open Types

open Lacamlext;;
open Lacaml.Z;;

type sim_type = 
  | Weak of int * (Mat.t * Mat.t) option * qtbl option
  | Strong of bool * (Mat.t * Mat.t) option

val rand_eval : ?shots:int -> 
                ?change_base:(Mat.t * Mat.t) option -> 
                ?qtbl:qtbl option -> 
                prog -> input_map -> (qubit list * (qubit, int) Hashtbl.t * Mat.t)

val simulate : ?just_prob:bool -> 
              ?change_base:(Mat.t * Mat.t) option -> 
              prog -> input_map -> (qubit list * (qubit, int) Hashtbl.t * Mat.t)

val run : sim_type -> prog -> input_map -> (qubit list * (qubit, int) Hashtbl.t * Mat.t)

val rand_eval_cmd_exec : ?mtbl_lock:Mutex.t option -> (qubit, int) Hashtbl.t -> qtbl -> Mat.t -> cmd -> Mat.t

