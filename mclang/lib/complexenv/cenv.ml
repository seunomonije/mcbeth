(**
  * Adds helper functions for dealing with complex numbers and
  * redefined arithmetic operators accordingly.
  * 
  * Uses the standard library Complex module.
  *
  *)

open Complex;;

(**
  * Creates a complex number.
  * val c : float -> float -> Complex.t
  *)
let c re im : t = {
  re = re;
  im = im;
};;

let float_to_complex f = c f 0.;;

(**
  * Converts a complex number to a string.
  *)
let cstr n = (
  let ftos x = Printf.sprintf "%.2f" x in
  "(" ^ (ftos n.re) ^ ", " ^ (ftos n.im) ^ ")"
);;

(**
  * Redefines arithmetic operators.
  *)
let ( + )   = add;; (* Addition *)
let ( - )   = sub;; (* Subtraction *)
let ( ~- )  = neg;; (* Unary Negation *)
let ( * )   = mul;; (* Multiplication *)
let ( / )   = div;; (* Division *)

(**
  * x ** y = Complex.pow x y
  * I.e., x to the power of y 
  *)
let ( ** )  = pow;; (* Exponentiation *)
