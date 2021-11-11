
open Types

val print_prog : prog -> unit
val cmd_to_string : cmd -> string

val calc_qubit_num : prog -> int

val well_formed : prog -> bool

val parse_pattern : pattern -> prog
val print_pattern : pattern -> unit

val get_output_qubits : prog -> (qubit, unit) Hashtbl.t

val standardize : prog -> prog
val expand_and_order_prep : prog -> prog

val performance : ('a -> 'b) -> 'a -> float

val unpack_qtbl : (qubit, int) Hashtbl.t -> qubit list
val print_qubits : qubit list -> unit
val print_readout : (qubit, int) Hashtbl.t -> unit
