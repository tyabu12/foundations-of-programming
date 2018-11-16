open Printf

(* 目的: 2次方程式 a*x^2 + bc + c = 0 の判別式の値を計算する *)
(* float -> float -> float -> float *)
let hanbetsushiki a b c =
  b ** 2. -. 4. *. a *. c

(* 目的: 2次方程式 a*x^2 + bc + c = 0 の解の個数を計算する *)
(* float -> float -> float -> int *)
let kai_no_kosuu a b c =
  let d = hanbetsushiki a b c in
  if d > 0. then 2
  else if d = 0. then 1
  else 0

(* テスト *)
let () =
  assert (kai_no_kosuu 2. 3. (-1.) = 2);
  assert (kai_no_kosuu 2. 4. 2. = 1);
  assert (kai_no_kosuu 4. 2. 1. = 0);
  ()

