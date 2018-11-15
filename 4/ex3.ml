open Printf

let hyojun_taiju h =
  h ** 2. *. 22.

let () =
  let println_float x = print_endline (string_of_float x) in
  println_float (hyojun_taiju 1.58);
  println_float (hyojun_taiju 1.68);
  println_float (hyojun_taiju 1.78);
  ()

