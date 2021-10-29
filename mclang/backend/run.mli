
open Types

open Lacamlext;;
open Lacaml.Z;;


val rand_eval : ?shots:int -> ?change_base:(Mat.t * Mat.t) option -> prog -> Mat.t

val simulate : ?just_prob:bool -> ?change_base:(Mat.t * Mat.t) option -> prog -> Mat.t
