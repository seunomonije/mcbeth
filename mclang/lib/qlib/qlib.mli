
open Lacamlext
open Lacaml.Z

module States : sig
  val zero_state_vec : Vec.t
  val one_state_vec : Vec.t
  val plus_state_vec : Vec.t
  val minus_state_vec : Vec.t
  val i_state_vec : Vec.t
  val negi_state_vec : Vec.t
  
  val zero_state : Mat.t
  val one_state : Mat.t
  val plus_state : Mat.t
  val minus_state : Mat.t
  val i_state : Mat.t
  val negi_state : Mat.t
end

module Bases : sig
  val x_basis : Mat.t * Mat.t
  val y_basis : Mat.t * Mat.t
  val z_basis : Mat.t * Mat.t
end

module Gates : sig
  val single_pauli_x : Mat.t
  val single_pauli_y : Mat.t
  val single_pauli_z : Mat.t

  val iden : int -> Mat.t

  val gate : Mat.t -> int -> int -> Mat.t
  val pauli_x : int -> int -> Mat.t
  val pauli_y : int -> int -> Mat.t
  val pauli_z : int -> int -> Mat.t

  val controlled : Mat.t -> int -> int -> int -> Mat.t
  val ctrl_x : int -> int -> int -> Mat.t
  val ctrl_y : int -> int -> int -> Mat.t
  val ctrl_z : int -> int -> int -> Mat.t

  val cnot : int -> int -> int -> Mat.t

  val to_all : Mat.t -> int -> Mat.t

  val change_base : ?qubits:int -> (Mat.t * Mat.t) -> (Mat.t * Mat.t) -> Mat.t
end

module DensityMatrix : sig
  val from_state_vector : Mat.t -> Mat.t
  val purity : Mat.t -> float
  val apply_operator : Mat.t -> Mat.t -> Mat.t
  val change_base : (Mat.t * Mat.t) -> (Mat.t * Mat.t) -> Mat.t -> int -> Mat.t
  val extract_info : ?print:bool -> Mat.t -> (int, float) Hashtbl.t

  module Measurement : sig
    val collapse : ?normalize:bool -> Mat.t -> Mat.t -> Mat.t
    val collapse_single : ?normalize:bool -> int -> int -> Mat.t -> Mat.t -> Mat.t
    val measure : (Mat.t * Mat.t) -> int -> int -> Mat.t -> (Mat.t * Mat.t)
  end
end

module StateVector : sig
  val to_density_matrix : Mat.t -> Mat.t
  val change_base : (Mat.t * Mat.t) -> (Mat.t * Mat.t) -> Mat.t -> int -> Mat.t
  val extract_info : ?print:bool -> Mat.t -> (int, float) Hashtbl.t

  module Measurement : sig
    val project : Mat.t -> Mat.t
    val collapse : Mat.t -> Mat.t -> Mat.t
    val collapse_single : ?proj_down:bool -> int -> int -> Mat.t -> Mat.t -> Mat.t
    val prob : Mat.t -> Mat.t -> float
    val prob_single : ?proj_down:bool -> int -> int -> Mat.t -> Mat.t -> float
    val measure : ?proj_down:bool -> (Mat.t * Mat.t) -> int -> int -> Mat.t -> (Mat.t * int)
  end
end
