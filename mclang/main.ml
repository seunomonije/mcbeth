type qubit = 
  | Meas of qubit
  | Entangle of qubit * qubit
  | XCorrect of qubit
  | ZCorrect of qubit
  | Value of string

let rec to_string e = 
  match e with 
  | Meas (value) ->
     "M[" ^ to_string value ^ "]"
  | Entangle (left, right) ->
    "E[" ^ to_string left ^ ", " ^ to_string right ^ "]"
  | XCorrect (value) ->
    "X[" ^ to_string value ^ "]"
  | ZCorrect (value) ->
    "Z[" ^ to_string value ^ "]"
  | Value v -> v;;

let print_expr e = 
  print_endline (to_string e);;


print_expr (XCorrect ((Value "1")));;