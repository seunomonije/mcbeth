
open Backend.Types;;
open Backend.Utils;;

let p = ([Prep(1); PrepList([0; 2]); Entangle(1, 0)]);;

print_prog p;;
print_endline(Int.to_string (if well_formed p then calc_qubit_num p else 0));;
print_endline("---------");;

let p = ([CInput(0, Plus); CInput(1, One); PrepList([2; 3]);
        Entangle(1, 0); Measure(1, 0.0, [], []); XCorrect(0, [])]);;

print_prog p;;
print_endline(Int.to_string (if well_formed p then calc_qubit_num p else 0));;
print_endline("---------");;

let p = ([CInput(0, Plus); CInput(1, Plus); PrepList([2; 3]);
        Entangle(1, 0); Measure(1, 0.0, [], []); XCorrect(0, [1])]);;

print_prog p;;
print_endline(Int.to_string (if well_formed p then calc_qubit_num p else 0));;
print_endline("---------");;

let p = ([CInput(0, Plus)]);;

print_prog p;;
print_endline(Int.to_string (if well_formed p then calc_qubit_num p else 0));;
print_endline("---------");;
