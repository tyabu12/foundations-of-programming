(* 多相木 *)
type 'a tree_t =
  | Empty                              (* 空の木 *)
  | Leaf of 'a                         (* 葉 *)
  | Node of 'a tree_t * 'a * 'a tree_t (* 節 *)

(* 目的: 木 tree に含まれる数をすべて加える *)
(* sum_tree : int tree_t -> int *)
(*
   問: なぜ多相にならないのか
   Empty の場合は 0 -> sum_tree の返り値は int と推論
   Node の場合に sum_tree の返り値と加算 → n も int
   以上より 'a = int と推論
 *)
let rec sum_tree tree = match tree with
  | Empty -> 0
  | Leaf n -> n
  | Node (l, n, r) -> sum_tree l + n + sum_tree r

(* テスト *)
let () =
  assert (sum_tree Empty = 0);
  assert (sum_tree (Leaf 3) = 3);
  let tree = Node (Empty, 3, Node ((Leaf 2), 1, Empty)) in
  assert (sum_tree tree = 6);
  ()
