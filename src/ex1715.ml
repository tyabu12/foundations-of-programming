open Metro
open Ex1010
open Ex1201 (* eki_t *)
open Ex1204
open Ex1412
open Ex1505
open Ex1710
open Ex1713
open Ex1714

(* 目的: ローマ字文字列の始点駅名 shiten_romaji と終点駅名 syuten_romaji を受け取り,
        最短経路を確定し, その中から終点駅のレコードを返す *)
(* dijkstra : string -> string -> eki_t *)
let dijkstra shiten_romaji syuten_romaji =
  (* 目的: 直前に確定した駅 p, 未確定の駅リスト v, 駅間リスト ekikan_lst を受け取り,
          必要な更新処理を行ったあとの未確定の駅のリストを返す *)
  (* koushin : eki_t -> eki_t list -> ekikan_t list -> eki_t list *)
  let koushin p v ekikan_lst =
    List.map (fun q ->
      let kyori = get_ekikan_kyori p.namae q.namae ekikan_lst in
      if kyori = infinity then q
      else if p.saitan_kyori +. kyori > q.saitan_kyori then q
      else
        {namae = q.namae; saitan_kyori = p.saitan_kyori +. kyori; temae_list = q.namae :: p.temae_list}
    ) v
  in
  (* 目的: 未確定の駅リスト v と駅間リスト ekikan_lst を受け取り,
          ダイクストラのアルゴリズムにしたがって,
          各駅について最短距離と最短経路が正しく入ったリストを返す    *)
  (* dijkstra_main : eki_t list -> ekikan_t list -> eki_t list *)
  let rec dijkstra_main v ekikan_lst = match v with
    | [] -> []
    | _ ->
      let (p, v') = saitan_wo_bunri v in
      p :: (dijkstra_main (koushin p v' ekikan_lst) ekikan_lst)
  in
  let ekimei_lst = seiretsu global_ekimei_list in
  let shiten_kanji = romaji_to_kanji shiten_romaji ekimei_lst in
  let syuten_kanji = romaji_to_kanji syuten_romaji ekimei_lst in
  let ekikan_list = inserts_ekikan Empty global_ekikan_list in
  let v = make_initial_eki_list ekimei_lst shiten_kanji in
  let u = dijkstra_main v ekikan_list in
  List.find (fun eki -> eki.namae = syuten_kanji) u

let time f title =
  let tmp = Sys.time () in
  f ();
  let t = Sys.time () -. tmp in
  print_endline (title ^ ": " ^ string_of_float t ^ " [sec]")

(* テスト *)
let () =
  let syuten_eki = (dijkstra "ikebukuro" "iidabashi") in
  assert (syuten_eki.namae = "飯田橋");
  assert (syuten_eki.temae_list = ["飯田橋"; "江戸川橋"; "護国寺"; "東池袋"; "池袋"]);
  assert (syuten_eki.saitan_kyori = 6.);
  (* リストと木で時間比較 *)
  time (fun _ -> let _ = Ex1605.dijkstra "ikebukuro" "iidabashi" in ()) "dijkstra (List)";
  time (fun _ -> let _ = dijkstra "ikebukuro" "iidabashi" in ()) "dijkstra (Binary Tree)";
  ()
