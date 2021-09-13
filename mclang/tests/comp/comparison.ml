
open Backend.Types;;
open Backend.Run;;

let foobar() = (
  print_endline("-- foobar test --");
  let p = ([Input(0, Minus); Input(1, Plus);
            (*Entangle(0, 1); Measure(0, 0.0, [], [])*)]) 
  in (
    let r_res = rand_eval ~shots:1000 p in
    let s_res = simulate ~just_prob:true p in
    (* Compare probability distributions of r_res to s_res. *)
    let _ = (r_res, s_res) in ()
  )
);;

let () = foobar();;


