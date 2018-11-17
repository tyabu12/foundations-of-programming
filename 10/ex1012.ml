open Printf
open Metro

(* 目的: ローマ字駅名 romaji_namae と駅名リスト ekimei_list を受け取り,
        その駅の漢字表記を返す *)
(* romaji_to_kanji : string -> ekimei_t list -> string *)
let rec romaji_to_kanji romaji_namae ekimei_list = match ekimei_list with
  | [] -> ""
  | hd :: tl -> if hd.romaji = romaji_namae then hd.kanji
                                            else romaji_to_kanji romaji_namae tl

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

(* 目的: ローマ字の駅名 namae1, namae2 を受け取り,
 *       直接つながっている場合は文字列「A駅からB駅までは?kmです」を,
 *       そうでないなら文字列「A駅とB駅はつながっていません」を返す *)
(* kyori_wo_hyoji : string -> string -> string *)
let kyori_wo_hyoji namae1 namae2 =
  let kanji_namae1 = romaji_to_kanji namae1 global_ekimei_list in
  let kanji_namae2 = romaji_to_kanji namae2 global_ekimei_list in
  let kyori = get_ekikan_kyori kanji_namae1 kanji_namae2 global_ekikan_list in
  if kyori = infinity then
    sprintf "%s駅と%s駅はつながっていません" kanji_namae1 kanji_namae2
  else
    sprintf "%s駅から%s駅までは%.1fkmです" kanji_namae1 kanji_namae2 kyori

(* テスト *)
let () =
  assert (kyori_wo_hyoji "" "" = "駅と駅はつながっていません");
  assert (kyori_wo_hyoji "myogadani" "shinotsuka" = "茗荷谷駅から新大塚駅までは1.2kmです");
  assert (kyori_wo_hyoji "shinotsuka" "myogadani" = "新大塚駅から茗荷谷駅までは1.2kmです");
  assert (kyori_wo_hyoji "toranomon" "tameikesanno" = "虎ノ門駅から溜池山王駅までは0.6kmです");
  assert (kyori_wo_hyoji "toranomon" "shinotsuka" = "虎ノ門駅と新大塚駅はつながっていません");
  ()

