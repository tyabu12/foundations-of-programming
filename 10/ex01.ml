(* 目的: 昇順の整数リスト l に昇順を崩さず整数 n を挿入したリストを返す *)
(* insert : int list -> int -> int list *)
let rec insert l n = match l with
    [] -> [n]
  | hd :: tl -> if hd >= n then n :: hd :: tl
                           else hd :: (insert tl n)

(* テスト *)
let () =
  assert (insert [] 1 = [1]);
  assert (insert [2] 1 = [1; 2]);
  assert (insert [2] 3 = [2; 3]);
  assert (insert [1; 3] 2 = [1; 2; 3]);
  assert (insert [1; 3; 5] 7 = [1; 3; 5; 7]);
  ()

