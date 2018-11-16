open Printf

(* 鶴の数nに応じた足の本数を計算する *)
(* int -> int *)
let tsuru_no_ashi n =
  n * 2

(* テスト *)
let () =
  assert (tsuru_no_ashi 1 = 2);
  assert (tsuru_no_ashi 3 = 6);
  assert (tsuru_no_ashi 8 = 16);
  ()

