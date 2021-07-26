
open Backend.Types;;
open Backend.Utils;;


let foobar() = (
  print_endline("-- foobar test --");
  let p = ([InitPlus(0); InitPlus(1)], 
            [Entangle(0, 1); Measure(0, 0.0, [], [])]) 
  in (
    print_endline (Int.to_string (calc_qubit_num p))
  )
);;

let () = foobar();;


