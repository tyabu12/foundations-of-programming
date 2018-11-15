open Printf

(* 目的: 五教科の点数 s1 s2 s3 s4 s5 に応じた合計点と平均点を計算する *)
(* int -> int -> int -> int -> int -> int * int *)
let goukei_to_heikin s1 s2 s3 s4 s5 =
  let sum = s1 + s2 + s3 + s4 + s5 in
  let ave = sum / 5 in
  (sum, ave)

let () =
  let println_bool x = print_endline (string_of_bool x) in
  (* テスト *)
  let test1 = goukei_to_heikin 1 2 3 4 5 = (15, 3) in
  let test2 = goukei_to_heikin 2 4 6 8 10 = (30, 6) in
  let test3 = goukei_to_heikin 90 65 50 78 65 = (348, 69) in
  println_bool test1;
  println_bool test2;
  println_bool test3;
  ()

