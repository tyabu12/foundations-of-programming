(* 目的: 五教科の点数 s1 s2 s3 s4 s5 に応じた合計点と平均点を計算する *)
(* int -> int -> int -> int -> int -> int * int *)
let goukei_to_heikin s1 s2 s3 s4 s5 =
  let sum = s1 + s2 + s3 + s4 + s5 in
  let ave = sum / 5 in
  (sum, ave)

(* テスト *)
let () =
  assert (goukei_to_heikin 1 2 3 4 5 = (15, 3));
  assert (goukei_to_heikin 2 4 6 8 10 = (30, 6));
  assert (goukei_to_heikin 90 65 50 78 65 = (348, 69));
  ()

