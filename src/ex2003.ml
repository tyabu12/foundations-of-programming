open Ex2001
open Ex2002

(* 目的: rb_tree_t 型の赤黒木, key, value を受け取り,それを挿入した赤黒木を返す.
        挿入する key がすでに木に存在する場合は値を更新する *)
(* insert : ('a, 'b) rb_tree_t -> 'a -> 'b -> ('a, 'b) rb_tree_t *)
let insert tree k v =
  let rec aux tr = match tr with
    | Empty -> Node (Empty, k, v, Red, Empty)
    | Node (left, key, value, color, right) ->
      if key = k then Node (left, key, v, color, right)
      else
        let tr' =
          if k < key then Node (aux left, key, value, color, right)
          else Node (left, key, value, color, aux right)
        in
        match color with
        | Red -> balance tr'
        | Black -> balance tr'
  in
  match aux tree with
  | Empty -> assert false (* この場合は起こりえない *)
  | Node (left, key, value, _, right) -> Node (left, key, value, Black, right)

(* テスト *)
let () =
  assert (insert Empty "k" "v" = Node (Empty, "k", "v", Black, Empty));
  let tree1 = List.fold_left
    (fun tr s -> insert tr (s ^ "k") (s ^ "v"))
    Empty
    ["a"; "b"; "c"; "d"; "e"; "f"; "g"]
  in
  let tree2 = List.fold_right
    (fun s tr -> insert tr (s ^ "k") (s ^ "v"))
    ["a"; "b"; "c"; "d"; "e"; "f"; "g"]
    Empty
  in
  let balanced_tree = Node(
    Node (
      Node (Empty, "ak", "av", Black, Empty),
      "bk", "bv", Black,
      Node (Empty, "ck", "cv", Black, Empty)),
    "dk", "dv", Black,
    Node (
      Node (Empty, "ek", "ev", Black, Empty),
      "fk", "fv", Black,
      Node (Empty, "gk", "gv", Black, Empty)))
  in
  assert (tree1 = balanced_tree);
  assert (tree2 = balanced_tree);
  ()
