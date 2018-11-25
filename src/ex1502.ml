(* 目的: 2つの自然数 m, n (m >= n >= 0) の最大公約数を求める *)
(* gcd : int -> int -> int *)
let rec gcd m n =
  assert (m >= n && n >= 0);
  if n = 0 then m
           else gcd n (m mod n) (* n > m mod n >= 0 *)

(* テスト *)
let () =
  assert (gcd 2 0 = 2);
  assert (gcd 3 2 = 1);
  assert (gcd 4 2 = 2);
  assert (gcd 5 2 = 1);
  assert (gcd 6 3 = 3);
  ()
