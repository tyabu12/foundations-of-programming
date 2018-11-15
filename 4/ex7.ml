open Printf

(* 鶴の数nに応じた足の本数を計算する *)
(* int -> int *)
let tsuru_no_ashi n =
  n * 2

(* 亀の数nに応じた足の本数を計算する *)
(* int -> int *)
let kame_no_ashi n =
  n * 4

(* 鶴の数tnと亀の数knに応じた足の本数を計算する *)
(* int -> int -> int *)
let tsurukame_no_ashi tn kn =
  (tsuru_no_ashi tn) + (kame_no_ashi kn)

let () =
  let println_bool x = print_endline (string_of_bool x) in
  (* テスト *)
  let test1 = tsurukame_no_ashi 1 1 = 6 in
  let test2 = tsurukame_no_ashi 3 2 = 14 in
  let test3 = tsurukame_no_ashi 8 4 = 32 in
  println_bool test1;
  println_bool test2;
  println_bool test3;
  ()

