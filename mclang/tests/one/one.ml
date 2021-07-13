
open Backend.Types;;
open Backend.Statevec;;

let foobar() = (
  print_endline("-- foobar test --");
  let p = ([Init0(0); Init1(1)], 
            [Measure(0, 0.0, [], [])]) 
  in let _ = rand_eval p in ()
);;

let () = foobar();;


