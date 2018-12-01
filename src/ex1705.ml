(* 整数木 *)
type tree_t =
  | Empty                         (* 空の木 *)
  | Leaf of int                   (* 葉 *)
  | Node of tree_t * int * tree_t (* 節 *)

(* 木 tree を受け取り, 節や葉に入っている値をすべて2倍にした木を返す *)
(* tree_double : tree_t -> tree_t *)
let rec tree_double tree = match tree with
  | Empty -> Empty
  | Leaf n -> Leaf (n * 2)
  | Node (l, n, r) -> Node (tree_double l, n * 2, tree_double r)

(* テスト *)
let () =
  assert (tree_double Empty = Empty);
  assert (tree_double (Leaf 0) = Leaf 0);
  assert (tree_double (Leaf 1) = Leaf 2);
  assert (tree_double (Node (Empty, 3, Node ((Leaf 2), 1, Empty)))
    = Node (Empty, 6, Node ((Leaf 4), 2, Empty)));
  ()
