(* 目的: n から 1 までのリストを作る *)
let rec enumerate n =
  if n = 0 then [] else n :: enumerate (n - 1)

(* 1から受け取った自然数 n までの合計を求める *)
(* one_to_n : int -> int *)
let one_to_n n = List.fold_right (+) (enumerate n) 0

(* テスト *)
let () =
  assert (one_to_n 1 = 1);
  assert (one_to_n 2 = 3);
  assert (one_to_n 3 = 6);
  assert (one_to_n 4 = 10);
  assert (one_to_n 10 = 55);
  ()
