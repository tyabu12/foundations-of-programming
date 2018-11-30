open Metro
open Ex1201 (* eki_t *)
open Ex1505
open Ex1603

(* 目的: 未確定の駅リスト v と駅間リスト ekikan_lst を受け取り,
        ダイクストラのアルゴリズムにしたがって,
        各駅について最短距離と最短経路が正しく入ったリストを返す    *)
(* dijkstra_main : eki_t list -> ekikan_t list -> eki_t list *)
let rec dijkstra_main v ekikan_lst = match v with
  | [] -> []
  | _ ->
    let (p, v') = saitan_wo_bunri v in
    p :: (dijkstra_main (koushin p v' ekikan_lst) ekikan_lst)

(* テスト *)
let () =
  assert (dijkstra_main [] global_ekikan_list = []);
  let eki_lst = [
    {namae="池袋"; saitan_kyori = infinity; temae_list = []};
    {namae="新大塚"; saitan_kyori = 1.2; temae_list = ["新大塚"; "茗荷谷"]};
    {namae="茗荷谷"; saitan_kyori = 0.; temae_list = ["茗荷谷"]};
    {namae="後楽園"; saitan_kyori = infinity; temae_list = []}
  ] in
  assert (
    dijkstra_main eki_lst global_ekikan_list = [
      {namae = "茗荷谷"; saitan_kyori = 0.; temae_list = ["茗荷谷"]};
      {namae = "新大塚"; saitan_kyori = 1.2; temae_list = ["新大塚"; "茗荷谷"]};
      {namae = "後楽園"; saitan_kyori = 1.8; temae_list = ["後楽園"; "茗荷谷"]};
      {namae = "池袋"; saitan_kyori = 3.; temae_list = ["池袋"; "新大塚"; "茗荷谷"]}
    ]
  );
  ()
