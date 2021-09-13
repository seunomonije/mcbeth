
open Backend.Types;;
open Backend.Utils;;

let programs = [

  [Prep(0); Input(1, One)];

  [Prep(0); Prep(1);
  Entangle(0, 1); Measure(0, 0.0, [], [])];

  [Prep(0); Prep(1);
  Entangle(0, 1); Measure(0, 0.0, [], []); Measure(1, 0.0, [], [])];

  [Input(0, Zero); PrepList([1; 2])] @
  parse_pattern [J(0.0, 0, 1); J(0.0, 1, 2)];

];;
