type color_t = Red | Black

type ('a, 'b) rb_tree_t =
  | Empty
  | Node of ('a, 'b) rb_tree_t * 'a * 'b * color_t * ('a, 'b) rb_tree_t

(* 目的: rb_tree_t 型の木を文字列に変換する (デバッグ用) *)
(* string_of_rb_tree : ('a, 'b) rb_tree_t -> ('a -> string) -> ('b -> string) -> string *)
let string_of_rb_tree tree string_of_key string_of_value =
  let rec aux tree = match tree with
  | Empty -> "()"
  | Node (left, key, value, color, right) ->
    "(" ^
      aux left ^ "," ^
      string_of_key key ^ "," ^
      string_of_value value ^ "," ^
      (if color = Black then "BLK" else "RED") ^ "," ^
      aux right ^
    ")"
  in aux tree

(* テスト *)
let () =
  let _ = Node (Empty, 1, "a", Red, Empty) in
  ()
