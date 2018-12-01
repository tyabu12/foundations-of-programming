open Metro
open Ex1710
open Ex1711

(* 目的: ekikan_tree_t 型の木 tree と駅間 ekikan を受け取り,
        その情報を挿入した木を返す *)
(* insert_ekikan : ekikan_tree_t -> ekikan_t -> ekikan_tree_t *)
let insert_ekikan tree ekikan =
  (* insert : ekikan_tree_t -> string -> string * float -> ekikan_tree_t *)
  let rec insert tr ekimei v = match tr with
    | Empty -> Node (Empty, ekimei, [v], Empty)
    | Node (l, s, lst, r) ->
      if s = ekimei then Node (l, s, v :: lst, r)
      else if s < ekimei then Node (insert l ekimei v, s, lst, r)
      else Node (l, s, lst, insert r ekimei v)
  in
  insert
    (insert
      tree
      ekikan.kiten
      (ekikan.shuten, ekikan.kyori))
    ekikan.shuten
    (ekikan.kiten, ekikan.kyori)

(* テスト *)
let () =
  let ekikan1 =
    {kiten="池袋"; shuten="新大塚"; keiyu="丸ノ内線"; kyori=1.8; jikan=3} in
  let ekikan2 =
    {kiten="新大塚"; shuten="茗荷谷"; keiyu="丸ノ内線"; kyori=1.2; jikan=2} in
  let ekikan3 =
    {kiten="茗荷谷"; shuten="後楽園"; keiyu="丸ノ内線"; kyori=1.8; jikan=2}  in
  let tree = insert_ekikan Empty ekikan1 in
  assert (tree = Node (
    Empty,
    "池袋", [("新大塚", 1.8)],
    Node (
      Empty,
      "新大塚", [("池袋", 1.8)],
      Empty
    )
  ));
  let tree' = insert_ekikan tree ekikan2 in
  assert (tree' = Node (
    Node (
      Empty,
      "茗荷谷", [("新大塚", 1.2)],
      Empty
    ),
    "池袋", [("新大塚", 1.8)],
    Node (
      Empty,
      "新大塚", [("茗荷谷", 1.2); ("池袋", 1.8)],
      Empty
    )
  ));
  let tree'' = insert_ekikan tree' ekikan3 in
  assert (tree'' = Node (
    Node (
      Empty,
      "茗荷谷", [("後楽園", 1.8); ("新大塚", 1.2)],
      Empty
    ),
    "池袋", [("新大塚", 1.8)],
    Node (
      Empty,
      "新大塚", [("茗荷谷", 1.2); ("池袋", 1.8)],
      Node (
        Empty,
        "後楽園", [("茗荷谷", 1.8)],
        Empty
      )
    )
  ));
  ()
