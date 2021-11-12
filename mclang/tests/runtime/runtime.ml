(*
open Backend.Run;;
*)
open Backend.Utils;;
open Backend.Distributed;;

open Lacaml.D;;

open Programs;;

let () = (
  let run func programs = (
    let total = 1 in
    let rec run n vec = (
      if (n > 0) then (
        let res = List.map (fun p -> (
          performance func p
        )) programs in
        run (n-1) (Vec.add vec (Vec.of_list res))
      ) else (
        let s = Float.div 1. (Float.of_int total) in
        Vec.map (fun e -> Float.mul e s) vec
      )
    ) in
    let res = Vec.to_list (run total (Vec.make (List.length programs) 0.)) in
    List.iter (fun e -> print_endline (Float.to_string e)) res;
    print_endline "---------------------"
  ) in
  let _ = (
    List.fold_left (fun ls x -> ls @ [linear x]) [] (List.init 0 (fun x -> x+1))
  ) in
  let _ = [parallel 1 1] in
  let dis = (
    let y = 3 in
    List.fold_left (fun ls x -> (
      print_endline (Int.to_string (x+1) ^ " " ^ Int.to_string y)
    ); ls @ [distributed_approx x y]) [] (List.init 2 (fun x -> x+10))
  ) in (
    (*
    run rand_eval lin;
    run rand_eval par;*)
    run run_dist_approx dis;
  )
)

