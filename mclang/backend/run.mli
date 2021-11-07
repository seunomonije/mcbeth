
open Types

open Lacamlext;;
open Lacaml.Z;;


val rand_eval : ?shots:int -> 
                ?change_base:(Mat.t * Mat.t) option -> 
                ?qtbl:qtbl option -> 
                prog -> (qubit list * Mat.t)

val simulate : ?just_prob:bool -> 
              ?change_base:(Mat.t * Mat.t) option -> 
              prog -> Mat.t

val rand_eval_cmd_exec : ?mtbl_lock:Mutex.t option -> (qubit, int) Hashtbl.t -> qtbl -> Mat.t -> cmd -> Mat.t

