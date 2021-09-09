open Base
open Python_lib
open Python_lib.Let_syntax

let approx_pi = 
  let%map_open n = positional "n" int ~docstring:"the value n" in
    let sum = 
      List.init n ~f:(fun i ->
        let i = Float.of_int(1 + i) in
        1.0 /. (i *. i))
        |> List.reduce_exn ~f:( +. )
      in
      Float.sqrt (sum *. 6.) |> python_of_float
    ;;

let () =
    if not (Py.is_initialized ()) then Py.initialize ();
    let mod_ = Py_module.create "mcl" in

    (* General test fns *)
    Py_module.set mod_ "approx_pi" approx_pi;
;;
