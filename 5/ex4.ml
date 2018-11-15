open Printf

(* 目的: 2次方程式 a*x^2 + bc + c = 0 の判別式の値を計算する *)
(* float -> float -> float -> float *)
let hanbetsushiki a b c =
  b ** 2. -. 4. *. a *. c

let () =
  let println_bool x = print_endline (string_of_bool x) in
  (* テスト *)
  let test1 = hanbetsushiki 2. 3. (-1.) = 17. in
  let test2 = hanbetsushiki 3. 6. 2. = 12. in
  let test3 = hanbetsushiki 4. 2. 1. = -12. in
  println_bool test1;
  println_bool test2;
  println_bool test3;
  ()

