open Ex1705

(* int -> int 型の関数 f と木 tree を受け取り, 節や葉に入っている値すべてに f を適用後の木を返す *)
(* tree_map : (int -> int) -> tree_t -> tree_t *)
let rec tree_map f tree = match tree with
  | Empty -> Empty
  | Leaf n -> Leaf (f n)
  | Node (l, n, r) -> Node (tree_map f l, f n, tree_map f r)

(* テスト *)
let () =
  let tree = Node (Empty, 3, Node ((Leaf 2), 1, Empty)) in
  assert (tree_map (fun x -> x) Empty = Empty);
  assert (tree_map (fun x -> x) tree = tree);
  assert (tree_map (fun x -> x * 2) tree = Ex1705.tree_double tree);
  ()
