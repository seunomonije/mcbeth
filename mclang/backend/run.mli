
open Types

open Lacamlext;;
open Lacaml.Z;;


val rand_eval : ?shots:int -> prog -> Mat.t

val simulate : ?just_prob:bool -> prog -> Mat.t
