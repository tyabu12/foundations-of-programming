(* 目的: 0から受け取った自然数 n までの2乗和を求める *)
(* sum_of_square : int -> int *)
let rec sum_of_square n =
  if n = 0 then 0
           else n * n + sum_of_square (n - 1)

(* テスト *)
let () =
  assert (sum_of_square 0 = 0);
  assert (sum_of_square 1 = 1);
  assert (sum_of_square 2 = 5);
  assert (sum_of_square 3 = 14);
  assert (sum_of_square 4 = 30);
  ()

