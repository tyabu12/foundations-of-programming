(* 目的: リスト l1 と l2 を受け取り、ふたつの長さが同じかどうかを判定する *)
(* equal_length : 'a list -> 'a list -> bool *)
let rec equal_length l1 l2 = match (l1, l2) with
    ([], []) -> true
  | ([], hd2 :: tl2) -> false
  | (hd1 :: tl1, []) -> false
  | (hd1 :: tl1, hd2 :: tl2) -> equal_length tl1 tl2

(* テスト *)
let () =
  assert (equal_length [] [] = true);
  assert (equal_length [1] [] = false);
  assert (equal_length [] ["a"] = false);
  assert (equal_length [1] ["a"] = true);
  assert (equal_length [1; 2] ["a"] = false);
  assert (equal_length [1; 2] ["a"; "b"] = true);
  ()

