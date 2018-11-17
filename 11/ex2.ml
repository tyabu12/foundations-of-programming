(* 目的: 次の漸化式で定義される数列a_nの第n項を求める
 *       a_0 = 3, a_n = 2 * a_(n-1) - 1 *)
(* a : int -> int *)
let rec a n =
  if n = 0 then 3
           else 2 * (a (n - 1)) - 1

let () =
  assert (a 0 = 3);
  assert (a 1 = 5);
  assert (a 2 = 9);
  assert (a 3 = 17);
  ()

