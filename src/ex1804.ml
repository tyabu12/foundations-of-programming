open Metro
open Ex1710
open Ex1803
open Ex1713
open CheckRaise

(* 目的: 漢字の駅名 namae1, namae2 と ekikan_tree_t 型の木 tree を
 *       受け取り, 駅間リストの中から二駅間の距離を返す *)
(* get_ekikan_kyori : string -> string -> ekikan_tree_t -> float *)
let rec get_ekikan_kyori namae1 namae2 tree = match tree with
  | Empty -> raise Not_found
  | Node (l, s, lst, r) ->
    if s = namae1 then assoc namae2 lst
    else if s < namae1 then get_ekikan_kyori namae1 namae2 l
    else get_ekikan_kyori namae1 namae2 r

(* テスト *)
let () =
  let ekikan_list = inserts_ekikan Empty global_ekikan_list in
  assert (check_raise (fun () -> get_ekikan_kyori "" "" Empty) Not_found);
  assert (check_raise (fun () -> get_ekikan_kyori "" "" ekikan_list) Not_found);
  assert (get_ekikan_kyori "茗荷谷" "新大塚" ekikan_list = 1.2);
  assert (get_ekikan_kyori "新大塚" "茗荷谷" ekikan_list = 1.2);
  assert (get_ekikan_kyori "虎ノ門" "溜池山王" ekikan_list = 0.6);
  assert (check_raise (fun () -> get_ekikan_kyori "虎ノ門" "新大塚" ekikan_list) Not_found);
  ()
