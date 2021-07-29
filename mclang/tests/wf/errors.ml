
open Backend.Types;;
open Backend.Utils;;

(* No Issues *)
print_endline(" -- No Issues -- ");;
let p = ([Input(1, Plus); PrepList([0; 2; 3]); Entangle(1, 0)]);;

print_prog p;;
well_formed p;;
print_endline("---------");;

let p = ([Input(0, Plus); Prep(1); PrepList([2; 3]); 
        Entangle(1, 0); Measure(1, 0.0, [], []); XCorrect(0, [])]);;

print_prog p;;
well_formed p;;
print_endline("---------");;

let p = ([Input(0, Plus); Input(1, Plus); PrepList([2; 3]); 
        Entangle(1, 0); Measure(1, 0.0, [], []); XCorrect(0, [1])]);;

print_prog p;;
well_formed p;;
print_endline("---------");;

let p = ([Prep(1); PrepList([2; 3]); Entangle(1, 2); Prep(0); Entangle(1, 0)]);;

print_prog p;;
well_formed p;;
print_endline("---------");;

(* Violates D5 *)
print_endline(" -- D5 Violation -- ");;
let p = ([Prep(1); PrepList([2; 3]); Entangle(1, 2)]);;

print_prog p;;
well_formed p;;
print_endline("---------");;


(* Violates D2 *)
print_endline(" -- D2 Violation -- ");;
let p = ([Prep(1); PrepList([2; 3]); Entangle(1, 0)]);;

print_prog p;;
well_formed p;;
print_endline("---------");;


(* Violates D1 *)
print_endline(" -- D1 Violation -- ");;
let p = ([Input(0, Plus); Prep(1); PrepList([2; 3]); 
        Entangle(1, 0); Measure(1, 0.0, [], []); XCorrect(1, [])]);;

print_prog p;;
well_formed p;;
print_endline("---------");;


(* Violates D0 *)
print_endline(" -- D0 Violation -- ");;
let p = ([Input(0, Minus); Prep(1); PrepList([2; 3]); 
        Entangle(1, 0); Measure(1, 0.0, [], []); XCorrect(0, [2])]);;

print_prog p;;
well_formed p;;
print_endline("---------");;



