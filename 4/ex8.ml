open Printf

(* 鶴と亀の数の合計nと足の数の合計mに応じた鶴の数を計算する *)
(* int -> int -> int *)
let tsurukame n m =
  (4 * n - m) / 2

let () =
  let println_bool x = print_endline (string_of_bool x) in
  (* テスト *)
  let test1 = tsurukame 2 6 = 1 in
  let test2 = tsurukame 5 14 = 3 in
  let test3 = tsurukame 12 32 = 8 in
  println_bool test1;
  println_bool test2;
  println_bool test3;
  ()

