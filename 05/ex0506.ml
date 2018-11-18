open Ex0504

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

