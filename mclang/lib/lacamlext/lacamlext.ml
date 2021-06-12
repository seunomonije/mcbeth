(* Linear Algebra Utils *)

module Lacaml = struct
  include Lacaml
  module Z = struct
    include Lacaml.Z
    
    module Vec = struct
      include Lacaml.Z.Vec

      let print v = (
        Format.printf "@[%a@]@\n@\n" Io.pp_cvec v;
      );;

      (**
        * Performs scalar multiplication on vector `v` by scalar `s`
        *)
      let scal_mul (s : Complex.t) (v : Vec.t) : Vec.t = (
        map (fun e -> Complex.mul e s) v
      );;

      (**
        * Calculates the tensor product of two vectors
        *)
      let tensor_prod v1 v2 = (
        let n = dim v2 in
        let m = dim v1 in
        let arr = Array.make m empty in
        let _ = fold (fun i e -> arr.(i) <- (mul (make n e) v2); i+1) 0 v1 in
        Array.fold_left (fun v1 v2 -> append v1 v2) empty arr
      );;

      let tensor_prod_list vs = (
        let open Complex in
        List.fold_left tensor_prod (make 1 { re = 1.; im = 0.; }) vs
      );;
      
      let tensor_prod_arr vs = (
        let open Complex in
        Array.fold_left tensor_prod (make 1 { re = 1.; im = 0.; }) vs
      );;

    end

    module Mat = struct
      include Lacaml.Z.Mat

      let print m = (
        Format.printf "@[%a@]@\n@\n" Io.pp_cmat m;
      );;

      (**
        * Performs scalar multiplication on matrix `m` by scalar `s`
        *)
      let scal_mul (s : Complex.t) (m : Mat.t) : Mat.t = (
        map (fun e -> Complex.mul e s) m
      );;

      (**
        * Calculates the tensor product of two matrices
        *)
      let tensor_prod m1 m2 = (
        let m1_arr = Array.concat (Array.to_list (to_array m1)) in
        let m2_arr = Array.concat (Array.to_list (to_array m2)) in
        let m1_rows = dim1 m1 in
        let m1_cols = dim2 m1 in
        let m2_rows = dim1 m2 in
        let m2_cols = dim2 m2 in
        let rows = m1_rows * m2_rows in
        let cols = m1_cols * m2_cols in
        let calc_m1_index = fun r c -> ((c-1) / m2_cols) + m1_cols * ((r-1) / m2_rows) in
        let calc_m2_index = fun r c -> ((c-1) mod m2_cols) + m2_rows * ((r-1) mod m2_rows) in
        init_rows rows cols (fun row col -> Complex.mul (m1_arr.(calc_m1_index row col)) (m2_arr.(calc_m2_index row col)))
        (* init_rows rows cols (fun row col -> { Complex.re = (Int.to_float (calc_m1_index row col)); im = (Int.to_float (calc_m2_index row col)); }) (* for debugging *) *)
      );;

      let tensor_prod_list ms = (
        let open Complex in
        List.fold_left tensor_prod (make 1 1 { re = 1.; im = 0.; }) ms
      );;
      
      let tensor_prod_arr ms = (
        let open Complex in
        Array.fold_left tensor_prod (make 1 1 { re = 1.; im = 0.; }) ms
      );;

    end  
  end
end;;
