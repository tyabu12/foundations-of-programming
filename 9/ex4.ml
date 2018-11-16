(* 目的: 受け取った整数リスト l の長さを返す *)
(* length : int list -> int *)
let rec length l = match l with
    [] -> 0
  | hd :: tl -> 1 + length tl

(* テスト *)
let () =
  assert (length [] = 0);
  assert (length [1] = 1);
  assert (length [1; 2] = 2);
  assert (length [1; 2; 3] = 3);
  ()

