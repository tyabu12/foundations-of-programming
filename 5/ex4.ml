open Printf

(* 目的: 2次方程式 a*x^2 + bc + c = 0 の判別式の値を計算する *)
(* float -> float -> float -> float *)
let hanbetsushiki a b c =
  b ** 2. -. 4. *. a *. c

(* テスト *)
let () =
  assert (hanbetsushiki 2. 3. (-1.) = 17.);
  assert (hanbetsushiki 3. 6. 2. = 12.);
  assert (hanbetsushiki 4. 2. 1. = -12.);
  ()

