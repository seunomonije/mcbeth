
let () = let open Cenv in (
  let x = c 1.0 2.0 in
  let y = c 1.0 (-.0.5) in
  print_endline (cstr (x + y))
)

