open Metro
open Ex1201 (* eki_t *)

(* 目的: 最短距離最小の駅の候補 eki と残りの駅リスト eki_t list を受け取り,
        「最短距離最小の駅」と「最短距離最小の駅以外からなるリスト」の組を返す *)
(* saitan_wo_bunri : eki_t list -> eki_t * eki_t list *)
let rec saitan_wo_bunri eki l =
  List.fold_right (fun e (min_eki, l') ->
    if e.saitan_kyori < min_eki.saitan_kyori then (e, min_eki :: l')
    else (min_eki, e :: l')
  ) l (eki, [])

(* テスト *)
let () =
  let sort = List.sort (fun e1 e2 -> if e1.saitan_kyori > e2.saitan_kyori then 1 else -1) in
  let eki1 = {namae="池袋"; saitan_kyori = 1.8; temae_list = ["新大塚"]} in
  let eki2 = {namae="新大塚"; saitan_kyori = 1.2; temae_list = ["茗荷谷"]} in
  let eki3 = {namae="四谷三丁目"; saitan_kyori = 0.9; temae_list = ["新宿御苑前"]} in
  let l = [eki2; eki3] in
  assert (saitan_wo_bunri eki1 [] = (eki1, []));
  assert (
    let (saitan_eki, l') = saitan_wo_bunri eki1 l in
    saitan_eki = eki3 && sort l' = sort [eki1; eki2]
  );
  ()
