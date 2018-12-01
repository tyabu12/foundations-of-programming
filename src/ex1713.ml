open Metro
open Ex1710
open Ex1712

(* 目的: ekikan_tree_t 型の木 tree と駅間リスト ekikan_lst を受け取り,
        リストの中に含まれる駅間をすべて挿入した木を返す *)
(* inserts_ekikan : ekikan_tree_t -> ekikan_t list -> ekikan_tree_t *)
let inserts_ekikan tree ekikan_lst =
  List.fold_right (fun ekikan tr -> insert_ekikan tr ekikan) ekikan_lst tree

let () =
  let ekikan1 =
    {kiten="池袋"; shuten="新大塚"; keiyu="丸ノ内線"; kyori=1.8; jikan=3} in
  let ekikan2 =
    {kiten="新大塚"; shuten="茗荷谷"; keiyu="丸ノ内線"; kyori=1.2; jikan=2} in
  let ekikan3 =
    {kiten="茗荷谷"; shuten="後楽園"; keiyu="丸ノ内線"; kyori=1.8; jikan=2}  in
  assert (inserts_ekikan Empty [] = Empty);
  assert (inserts_ekikan Empty [ekikan1] = insert_ekikan Empty ekikan1);
  assert (inserts_ekikan Empty [ekikan1; ekikan2] =
    insert_ekikan
      (insert_ekikan Empty ekikan2)
      ekikan1
  );
  assert (inserts_ekikan Empty [ekikan1; ekikan2; ekikan3] =
    insert_ekikan
      (insert_ekikan
        (insert_ekikan Empty ekikan3)
        ekikan2)
      ekikan1
  );
  ()
