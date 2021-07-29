
open Backend.Types;;
open Backend.Utils;;


let foobar() = (
  print_endline("-- foobar test --");
  let p = ([Input(0, Plus); Input(1, Plus);
            Entangle(0, 1); Measure(0, 0.0, [], [])]) 
  in (
    print_endline (Int.to_string (calc_qubit_num p))
  )
);;

let () = foobar();;


