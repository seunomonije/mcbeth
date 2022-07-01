
open Backend.Utils;;

open Backend.Run;;

open Lacamlext;;
open Lacaml.Z;;

open Algos.Create;;

let foobar() = (
  let p = (
    teleport 0 1 2
  ) in (
    print_prog p; print_endline "";
    let qs, readout, res = run (Weak(0, None, None)) p [(0, Plus)] in (
      print_qubits qs;
      print_readout readout;
      Mat.print res
    )
  )
)

let _ = foobar();;


