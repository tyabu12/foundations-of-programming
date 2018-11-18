open Metro

(* 目的: 漢字の駅名 namae1, namae2 と駅間リスト ekikan_list を
 *       受け取り, 駅間リストの中から二駅間の距離を返す *)
(* get_ekikan_kyori : string -> string -> ekikan_t list -> float *)
let rec get_ekikan_kyori namae1 namae2 ekikan_list = match ekikan_list with
  | [] -> infinity
  | {kiten=kiten; shuten=shuten; kyori=kyori} :: tl
     -> if (kiten = namae1 && shuten = namae2)
          || (kiten = namae2 && shuten = namae1) then
          kyori
        else
          get_ekikan_kyori namae1 namae2 tl

(* テスト *)
let () =
  let ekikan_list = global_ekikan_list in
  assert (get_ekikan_kyori "" "" [] = infinity);
  assert (get_ekikan_kyori "" "" ekikan_list = infinity);
  assert (get_ekikan_kyori "茗荷谷" "新大塚" ekikan_list = 1.2 );
  assert (get_ekikan_kyori "新大塚" "茗荷谷" ekikan_list = 1.2 );
  assert (get_ekikan_kyori "虎ノ門" "溜池山王" ekikan_list = 0.6 );
  assert (get_ekikan_kyori "虎ノ門" "新大塚" ekikan_list = infinity );
  ()

