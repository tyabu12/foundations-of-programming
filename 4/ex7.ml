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

(* テスト *)
let () =
  assert (tsurukame_no_ashi 1 1 = 6);
  assert (tsurukame_no_ashi 3 2 = 14);
  assert (tsurukame_no_ashi 8 4 = 32);
  ()

