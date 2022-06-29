
open Backend.Run;;

open Lacamlext;;
open Lacaml.Z;;

let () = List.iter (fun p -> (
  Mat.print (
    (simulate p ~just_prob:true)
  )
)) Programs.programs

