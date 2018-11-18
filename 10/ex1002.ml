open Ex1001

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

