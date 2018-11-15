open Printf

let bmi h w =
  w /. (h ** 2.)

let () =
  let println_float x = print_endline (string_of_float x) in
  println_float (bmi 1.58 55.);
  println_float (bmi 1.68 55.);
  println_float (bmi 1.78 55.);
  ()

