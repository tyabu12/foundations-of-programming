open Metro
open Ex1201 (* eki_t *)
open Ex1204
open Ex1010
open Ex1412
open Ex1604

(* 目的: ローマ字文字列の始点駅名 shiten_romaji と終点駅名 syuten_romaji を受け取り,
        最短経路を確定し, その中から終点駅のレコードを返す *)
(* dijkstra : string -> string -> eki_t *)
let dijkstra shiten_romaji syuten_romaji =
  let ekimei_lst = seiretsu global_ekimei_list in
  let shiten_kanji = romaji_to_kanji shiten_romaji ekimei_lst in
  let syuten_kanji = romaji_to_kanji syuten_romaji ekimei_lst in
  let v = make_initial_eki_list ekimei_lst shiten_kanji in
  let u = dijkstra_main v global_ekikan_list in
  List.hd (List.filter (fun eki -> eki.namae = syuten_kanji) u)

(* テスト *)
let () =
  let syuten_eki = (dijkstra "ikebukuro" "iidabashi") in
  assert (syuten_eki.namae = "飯田橋");
  assert (syuten_eki.temae_list = ["飯田橋"; "江戸川橋"; "護国寺"; "東池袋"; "池袋"]);
  assert (syuten_eki.saitan_kyori = 6.);
  ()
