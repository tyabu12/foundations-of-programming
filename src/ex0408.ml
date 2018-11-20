(* 鶴と亀の数の合計nと足の数の合計mに応じた鶴の数を計算する *)
(* int -> int -> int *)
let tsurukame n m =
  (4 * n - m) / 2

(* テスト *)
let () =
  assert (tsurukame 2 6 = 1);
  assert (tsurukame 5 14 = 3);
  assert (tsurukame 12 32 = 8);
  ()

