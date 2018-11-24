(* 目的: 受け取った整数リスト l から偶数の要素のみのリストを返す *)
(* even: int list -> int list *)
let rec even l =
  let is_even n = n mod 2 = 0 in
  List.filter is_even l

(* テスト *)
let () =
  assert (even [] = []);
  assert (even [1] = []);
  assert (even [2] = [2]);
  assert (even [1; 4] = [4]);
  assert (even [10; 3] = [10]);
  assert (even [1; 0; 10; 5; 11] = [0; 10]);
  ()
