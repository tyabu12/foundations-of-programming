open Ex2001

(* 目的: rb_tree_t 型の木を受け取り, 必要なら木を再構成する *)
(* balance : ('a, 'b) rb_tree_t -> ('a, 'b) rb_tree_t *)
let rec balance tree = match tree with
  | Node (
      Node (
        Node (a, xk, xv, Red, b),
        yk, yv, Red,
        c),
      zk, zv, Black,
      d)
  | Node (
      Node (
        a,
        xk, xv, Red,
        Node (b, yk, yv, Red, c)),
      zk, zv, Black,
      d)
  | Node (
      a,
      xk, xv, Black,
      Node (
        Node (b, yk, yv, Red, c),
        zk, zv, Red,
        d))
  | Node (
      a,
      xk, xv, Black,
      Node (
        b,
        yk, yv, Red,
        Node (c, zk, zv, Red, d)))
    -> Node (
        Node (a, xk, xv, Black, b),
        yk, yv, Red,
        Node (c, zk, zv, Black, d))
  | _ -> tree

(* テスト *)
let () =
  let tree1 = Node (
    Node (
      Node (Empty, "xk", "xv", Red, Empty),
      "yk", "yv", Red,
      Empty),
    "zk", "zv", Black,
    Empty)
  in
  let tree2 = Node (
    Node (
      Empty,
      "xk", "xv", Red,
      Node (Empty, "yk", "yv", Red, Empty)),
    "zk", "zv", Black,
    Empty)
  in
  let tree3 = Node (
    Empty,
    "xk", "xv", Black,
    Node (
      Node (Empty, "yk", "yv", Red, Empty),
      "zk", "zv", Red,
      Empty))
  in
  let tree4 = Node (
    Empty,
    "xk", "xv", Black,
    Node (
      Empty,
      "yk", "yv", Red,
      Node (Empty, "zk", "zv", Red, Empty)))
  in
  let balanced_tree = Node (
    Node (Empty, "xk", "xv", Black, Empty),
    "yk", "yv", Red,
    Node (Empty, "zk", "zv", Black, Empty))
  in
  assert (balance Empty = Empty);
  assert (balance tree1 = balanced_tree);
  assert (balance tree2 = balanced_tree);
  assert (balance tree3 = balanced_tree);
  assert (balance tree4 = balanced_tree);
  ()
