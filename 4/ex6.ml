open Printf

(* 鶴の数nに応じた足の本数を計算する *)
(* int -> int *)
let tsuru_no_ashi n =
  n * 2

let () =
  let println_bool x = print_endline (string_of_bool x) in
  (* テスト *)
  let test1 = tsuru_no_ashi 1 = 2 in
  let test2 = tsuru_no_ashi 3 = 6 in
  let test3 = tsuru_no_ashi 8 = 16 in
  println_bool test1;
  println_bool test2;
  println_bool test3;
  ()

