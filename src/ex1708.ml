open Ex1705

(* 木 tree を受け取り, 木の深さを返す *)
(* tree_depth : tree_t -> int *)
let rec tree_depth tree = match tree with
  | Empty -> 0
  | Leaf _ -> 1
  | Node (l, _, r) -> 1 + max (tree_depth l) (tree_depth r)

(* テスト *)
let () =
  assert (tree_depth Empty = 0);
  assert (tree_depth (Leaf 3) = 1);
  let tree = Node (Empty, 3, Node ((Leaf 2), 1, Empty)) in
  assert (tree_depth tree = 3);
  ()
