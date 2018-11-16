open Printf

(* 目的: 2次方程式 a*x^2 + bc + c = 0 の判別式の値を計算する *)
(* float -> float -> float -> float *)
let hanbetsushiki a b c =
  b ** 2. -. 4. *. a *. c

(* 目的: 2次方程式 a*x^2 + bc + c = 0 が虚数解を持つか判定する *)
(* float -> float -> float -> bool *)
let kyosuukai a b c =
  hanbetsushiki a b c < 0.

(* テスト *)
let () =
  assert (kyosuukai 2. 3. (-1.) = false);
  assert (kyosuukai 2. 4. 2. = false);
  assert (kyosuukai 4. 2. 1. = true);
  ()

