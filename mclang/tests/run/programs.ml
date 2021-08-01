
open Backend.Types;;

let programs = [

  [Prep(0); Input(1, One)];

  [Prep(0); Prep(1);
  Entangle(0, 1); Measure(0, 0.0, [], [])];

];;
