
open Backend.Types;;
open Backend.Run;;

let foobar() = (
  print_endline("-- foobar test --");
  let p = ([InitPlus(0); InitPlus(1)], 
            [Entangle(0, 1); Measure(0, 0.0, [], [])]) 
  in let _ = rand_eval p in ()
);;

let () = foobar();;


