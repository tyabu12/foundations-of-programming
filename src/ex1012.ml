open Printf
open Metro
open Ex1010
open Ex1011

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

