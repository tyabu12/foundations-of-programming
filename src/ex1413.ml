open Metro
open Ex1011
open Ex1201 (* eki_t *)

(* 目的: 直前に確定した駅 p と未確定の駅リスト v を受け取り,
        必要な更新処理を行ったあとの未確定の駅のリストを返す *)
(* koushin : eki_t -> eki_t list -> eki_t list *)
let koushin p v =
  List.map (fun q ->
    let kyori = get_ekikan_kyori p.namae q.namae global_ekikan_list in
    if kyori = infinity then q
    else
      {namae = q.namae;
      saitan_kyori = if kyori < q.saitan_kyori then kyori
                                                else q.saitan_kyori;
      temae_list = p.namae :: q.temae_list}
  ) v

(* テスト *)
let () =
  let p = {namae = "明治神宮前"; saitan_kyori = 0.; temae_list = []} in
  let v = [
    {namae = "表参道"; saitan_kyori = infinity; temae_list = []};
    {namae = "中目黒"; saitan_kyori = infinity; temae_list = []};
    {namae = "代々木公園"; saitan_kyori = infinity; temae_list = []};
  ] in
  assert (koushin p v = [
    {namae = "表参道"; saitan_kyori = 0.9; temae_list = ["明治神宮前"]};
    {namae = "中目黒"; saitan_kyori = infinity; temae_list = []};
    {namae = "代々木公園"; saitan_kyori = 1.2; temae_list = ["明治神宮前"]};
  ]);
  ()
