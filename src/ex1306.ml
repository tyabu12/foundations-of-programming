open Metro
open Ex1011
open Ex1201 (* eki_t *)

(* 目的: 直前に確定した駅 p と未確定の駅 q を受け取り,
 *      p と q が直接つながっているかどうかを調べ,
 *      つながっていたら q の最短距離と手前リストを必要に応じて更新したもの,
 *      つながっていなかったらもとの q をそのまま帰す
 *)
(* koushin1 : eki_t -> eki_t -> eki_t *)
let koushin1 p q =
  let kyori = get_ekikan_kyori p.namae q.namae global_ekikan_list in
  if kyori = infinity then q
  else
    {namae = q.namae;
     saitan_kyori = if kyori < q.saitan_kyori then kyori
                                              else q.saitan_kyori;
     temae_list = p.namae :: q.temae_list}

(* テスト *)
let () =
  let p = {namae = "明治神宮前"; saitan_kyori = 0.; temae_list = []} in
  let q1 = {namae = "表参道"; saitan_kyori = infinity; temae_list = []} in
  let q2 = {namae = "中目黒"; saitan_kyori = infinity; temae_list = []} in
  let q3 = {namae = "代々木公園"; saitan_kyori = infinity; temae_list = []} in
  assert (koushin1 p q1 = {namae = "表参道"; saitan_kyori = 0.9; temae_list = ["明治神宮前"]});
  assert (koushin1 p q2 = {namae = "中目黒"; saitan_kyori = infinity; temae_list = []});
  assert (koushin1 p q3 = {namae = "代々木公園"; saitan_kyori = 1.2; temae_list = ["明治神宮前"]});
  ()
