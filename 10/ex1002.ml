(* 目的: 昇順の整数リスト l に昇順を崩さず整数 n を挿入したリストを返す *)
(* insert : int list -> int -> int list *)
let rec insert l n = match l with
    [] -> [n]
  | hd :: tl -> if hd >= n then n :: hd :: tl
                           else hd :: (insert tl n)

(* 目的: 整数リスト l を昇順に整列したリストを返す *)
(* ins_sort : int list -> int list *)
let rec ins_sort l = match l with
    [] -> []
  | hd :: tl -> insert (ins_sort tl) hd

(* テスト *)
let () =
  assert (ins_sort [] = []);
  assert (ins_sort [2] = [2]);
  assert (ins_sort [1; 2] = [1; 2]);
  assert (ins_sort [2; 1] = [1; 2]);
  assert (ins_sort [10; 2; 5; 1; -1; 0] = [-1; 0; 1; 2; 5; 10]);
  ()

