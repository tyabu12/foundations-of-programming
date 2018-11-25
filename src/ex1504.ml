open Metro
open Ex1201 (* eki_t *)

(* 目的: eki_t list を受け取り, 「最短距離最小の駅」と「最短距離最小の駅以外からなるリスト」の組を返す *)
(* saitan_wo_bunri : eki_t list -> eki_t * eki_t list *)
let rec saitan_wo_bunri l = match l with
  | [] -> ({namae = ""; saitan_kyori = infinity; temae_list = []}, [])
  | hd :: [] -> (hd, [])
  | hd :: tl ->
    let (min_eki, l') = saitan_wo_bunri tl in
    if hd.saitan_kyori < min_eki.saitan_kyori then (hd, min_eki :: l')
                                              else (min_eki, hd :: l')
(* テスト *)
let () =
  let sort = List.sort (fun e1 e2 -> if e1.saitan_kyori > e2.saitan_kyori then 1 else -1) in
  let eki1 = {namae="池袋"; saitan_kyori = 1.8; temae_list = ["新大塚"]} in
  let eki2 = {namae="新大塚"; saitan_kyori = 1.2; temae_list = ["茗荷谷"]} in
  let eki3 = {namae="四谷三丁目"; saitan_kyori = 0.9; temae_list = ["新宿御苑前"]} in
  let l = [eki1; eki2; eki3] in
  assert (saitan_wo_bunri [] = ({namae = ""; saitan_kyori = infinity; temae_list = []}, []));
  assert (
    let (saitan_eki, l') = saitan_wo_bunri l in
    saitan_eki = eki3 && sort l' = sort [eki1; eki2]
  );
  ()
