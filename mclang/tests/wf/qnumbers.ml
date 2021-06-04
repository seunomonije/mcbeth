
open Backend.Types;;
open Backend.Run;;

let p = ([Init(1, 0.375324); InitNonInput([0; 2])], [Entangle(1, 0)]);;

print_prog p;;
print_endline(Int.to_string (well_formed p));;
print_endline("---------");;

let p = ([InitPlus(0); Init(1, 0.375324); InitNonInput([2; 3])], 
        [Entangle(1, 0); Measure(1, 0.0, [], []); XCorrect(0, [])]);;

print_prog p;;
print_endline(Int.to_string (well_formed p));;
print_endline("---------");;

let p = ([InitPlus(0); Init(1, 0.375324); InitNonInput([2; 3])], 
        [Entangle(1, 0); Measure(1, 0.0, [], []); XCorrect(0, [1])]);;

print_prog p;;
print_endline(Int.to_string (well_formed p));;
print_endline("---------");;

let p = ([InitPlus(0)], 
        []);;

print_prog p;;
print_endline(Int.to_string (well_formed p));;
print_endline("---------");;
