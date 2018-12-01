open Metro
open Ex1201
open Ex1710
open Ex1713
open Ex1804
open CheckRaise

(* 目的: 直前に確定した駅 p, 未確定の駅リスト v, 駅間リスト ekikan_lst を受け取り,
        必要な更新処理を行ったあとの未確定の駅のリストを返す *)
(* koushin : eki_t -> eki_t list -> ekikan_t list -> eki_t list *)
let koushin p v ekikan_lst =
  List.map (fun q ->
    try
      let kyori = get_ekikan_kyori p.namae q.namae ekikan_lst in
      if p.saitan_kyori +. kyori > q.saitan_kyori then q
      else
        {namae = q.namae; saitan_kyori = p.saitan_kyori +. kyori; temae_list = q.namae :: p.temae_list}
    with Not_found -> q
  ) v

(* テスト *)
let () =
  let p = {namae = "明治神宮前"; saitan_kyori = 0.; temae_list = ["明治神宮前"]} in
  let v = [
    {namae = "表参道"; saitan_kyori = infinity; temae_list = []};
    {namae = "中目黒"; saitan_kyori = infinity; temae_list = []};
    {namae = "代々木公園"; saitan_kyori = infinity; temae_list = []};
  ] in
  let ekikan_lst = inserts_ekikan Empty global_ekikan_list in
  assert (koushin p v ekikan_lst = [
    {namae = "表参道"; saitan_kyori = 0.9; temae_list = ["表参道"; "明治神宮前"]};
    {namae = "中目黒"; saitan_kyori = infinity; temae_list = []};
    {namae = "代々木公園"; saitan_kyori = 1.2; temae_list = ["代々木公園"; "明治神宮前"]};
  ]);
  ()
