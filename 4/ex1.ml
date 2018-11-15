open Printf

let baito_kyuyo y h =
  (850 + (y * 100))

let () =
  let println_int x = print_endline (string_of_int x) in
  println_int (baito_kyuyo 0 3);
  println_int (baito_kyuyo 1 3);
  println_int (baito_kyuyo 2 3);
  println_int (baito_kyuyo 3 3);
  ()

