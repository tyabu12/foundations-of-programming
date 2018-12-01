(* 二分探索木を表すモジュール *)
module Tree_t : sig

  (* キーが 'a 型, 値が 'b 型の木の型 *)
  type ('a, 'b) t

  (* empty *)
  (* 空の木 *)
  val empty : ('a, 'b) t

  (* insert tree key value *)
  (* 木 tree に key と 値 value を挿入した木を返す *)
  val insert : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t

  (* search tree key *)
  (* 木 tree の中から key に対応する値を探して返す *)
  val search : ('a, 'b) t -> 'a -> 'b

end = struct

  (* 二分探索木を表す型 *)
  type ('a, 'b) t =
    | Empty
    | Node of ('a, 'b) t * 'a * 'b * ('a, 'b) t

  (* 空の木 *)
  let empty = Empty

  (* 木 tree に key と 値 value を挿入した木を返す *)
  let rec insert tree k v = match tree with
    | Empty -> Node (Empty, k, v, Empty)
    | Node (left, key, value, right) ->
      if k = key then Node (left, key, v, right)
      else if k < key then Node (insert left k v, key, value, right)
      else Node (left, key, value, insert right k v)

  (* 木 tree の中から key に対応する値を探して返す *)
  let rec search tree k = match tree with
    | Empty -> raise Not_found
    | Node (left, key, value, right) ->
      if k = key then value
      else if k < key then search left k
      else search right k

end

open Metro
open Ex1803
open Ex1713
open CheckRaise

(* 目的: 漢字の駅名 namae1, namae2 と ekikan_tree_t 型の木 tree を
 *       受け取り, 駅間リストの中から二駅間の距離を返す *)
(* get_ekikan_kyori : string -> string -> ekikan_tree_t -> float *)
let rec get_ekikan_kyori namae1 namae2 tree =
  List.assoc namae2 (Tree_t.search tree namae1)

(* テスト *)
let () =
  let ekikan_tree =
    Tree_t.insert
      (Tree_t.insert
        (Tree_t.insert
          (Tree_t.insert Tree_t.empty "池袋" [("新大塚", 1.8)])
          "茗荷谷" [("後楽園", 1.8); ("新大塚", 1.2)])
        "新大塚" [("茗荷谷", 1.2); ("池袋", 1.8)])
      "後楽園" [("茗荷谷", 1.8)]
  in
  assert (check_raise (fun () -> get_ekikan_kyori "" "" Tree_t.empty) Not_found);
  assert (check_raise (fun () -> get_ekikan_kyori "" "" ekikan_tree) Not_found);
  assert (get_ekikan_kyori "池袋" "新大塚" ekikan_tree = 1.8);
  assert (get_ekikan_kyori "茗荷谷" "新大塚" ekikan_tree = 1.2);
  assert (get_ekikan_kyori "新大塚" "茗荷谷" ekikan_tree = 1.2);
  assert (check_raise (fun () -> get_ekikan_kyori "虎ノ門" "新大塚" ekikan_tree) Not_found);
  ()
