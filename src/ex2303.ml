open Metro
open RedBlack
open Ex1010
open Ex1201 (* eki_t *)
open Ex1204
open Ex2301

(* t -> ekikan_t list -> t *)
let inserts_ekikan tree ekikan_lst =
  List.fold_right (fun ekikan tr ->
    let l = try search tr ekikan.kiten with Not_found -> [] in
    let v k = (k, ekikan.kyori) :: l in
    let tr = insert tr ekikan.kiten (v ekikan.shuten) in
    insert tr ekikan.shuten (v ekikan.kiten)
  ) ekikan_lst tree

(* 目的: 漢字の駅名 namae1, namae2 と木 tree を
 *       受け取り, 駅間リストの中から二駅間の距離を返す *)
(* string -> string -> t -> float *)
let rec get_ekikan_kyori namae1 namae2 tree =
  List.assoc namae2 (search tree namae1)

(* 目的: ローマ字文字列の始点駅名 shiten_romaji と終点駅名 syuten_romaji を受け取り,
        最短経路を確定し, その中から終点駅のレコードを返す *)
(* string -> string -> eki_t *)
let dijkstra shiten_romaji syuten_romaji =

  (* 目的: ekimei_t list 型のリスト l と漢字の駅名 shiten_namae を受け取り,
  *       その駅名に応じた駅一覧を返す *)
  (* ekimei_t list -> string -> float * eki_t heap *)
  let make_initial_eki_heap l shiten_namae =
    let h = Heap.create (List.length l) 0.
      {namae = "";  saitan_kyori = 0.; temae_list = []}
    in
    let insert ekimei saitan_kyori temae_list =
      Heap.insert h saitan_kyori {namae=ekimei.kanji; saitan_kyori; temae_list}
    in
    List.iter (fun ekimei ->
      let _ =
        if ekimei.kanji = shiten_namae then
          insert ekimei 0. [shiten_namae]
        else
          insert ekimei infinity []
      in ()
    ) l;
    h
  in

  (* 目的: 駅一覧 v を受け取り, 「最短距離最小の駅」と「最短距離最小の駅一覧」の組を返す *)
  (* eki_t heap -> eki_t * eki_t heap *)
  let saitan_wo_bunri v =
    let (_, value), v = Heap.split_top v in (value, v)
  in

  (* 目的: 直前に確定した駅 p, 未確定の駅一覧 v, 駅間リスト ekikan_lst を受け取り,
          必要な更新処理を行ったあとの未確定の駅のリストを返す *)
  (* eki_t -> eki_t heap -> ((string * string, float) list) t -> eki_t heap *)
  let koushin p v ekikan_lst =
    Heap.map (fun _ q ->
      let r =
        try
          let kyori = get_ekikan_kyori p.namae q.namae ekikan_lst in
          if p.saitan_kyori +. kyori > q.saitan_kyori then q
          else
            {namae = q.namae; saitan_kyori = p.saitan_kyori +. kyori; temae_list = q.namae :: p.temae_list}
        with Not_found -> q
      in (r.saitan_kyori, r)
    ) v
  in

  (* 目的: 未確定の駅一覧 v と駅間リスト ekikan_lst を受け取り,
          ダイクストラのアルゴリズムにしたがって,
          各駅について最短距離と最短経路が正しく入ったリストを返す    *)
  (* eki_t heap -> ((string * (string * float)) list) t -> eki_t list *)
  let rec dijkstra_main v ekikan_lst =
    if Heap.length v = 0 then []
    else
      let p, v = saitan_wo_bunri v in
      p :: (dijkstra_main (koushin p v ekikan_lst) ekikan_lst)
  in
  let ekimei_lst = seiretsu global_ekimei_list in
  let shiten_kanji = romaji_to_kanji shiten_romaji ekimei_lst in
  let syuten_kanji = romaji_to_kanji syuten_romaji ekimei_lst in
  let ekikan_list = inserts_ekikan empty global_ekikan_list in
  let v = make_initial_eki_heap ekimei_lst shiten_kanji in
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
  time (fun _ -> let _ = dijkstra "ikebukuro" "iidabashi" in ()) "dijkstra (RedBlack Tree, Heap Tree)";
  ()
