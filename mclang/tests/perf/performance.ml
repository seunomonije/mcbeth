
open Backend.Run;;
open Backend.Utils;;

open Lacaml.D;;

open Programs;;

let () = (
  let func = simulate in
  let total = 1 in
  let rec run n vec = (
    if (n > 0) then (
      let res = List.map (fun p -> (
        let p' = standardize p in
        performance func p'
      )) programs in
      run (n-1) (Vec.add vec (Vec.of_list res))
    ) else (
      let s = Float.div 1. (Float.of_int total) in
      Vec.map (fun e -> Float.mul e s) vec
    )
  ) in
  let res = Vec.to_list (run total (Vec.make (List.length programs) 0.)) in
  List.iter (fun e -> print_endline (Float.to_string e)) res
)

