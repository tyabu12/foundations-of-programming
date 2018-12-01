open Ex1705

(* 木 tree を受け取り, 節と葉が合計いくつあるかを返す *)
(* tree_length : tree_t -> int *)
let rec tree_length tree = match tree with
  | Empty -> 0
  | Leaf _ -> 1
  | Node (l, _, r) -> 1 + tree_length l + tree_length r

(* テスト *)
let () =
  assert (tree_length Empty = 0);
  assert (tree_length (Leaf 3) = 1);
  let tree = Node (Empty, 3, Node ((Leaf 2), 1, Empty)) in
  assert (tree_length tree = 3);
  ()
