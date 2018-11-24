open Metro
open Ex1201 (* eki_t *)

(* 目的: ekimei_t list 型のリスト l と漢字の駅名 shiten_namae を受け取り,
 *       その駅名に応じた eki_t list を返す *)
(* make_initial_eki_list: ekimei_t list -> string -> eki_t list *)
let make_initial_eki_list l shiten_namae =
  List.map (fun ekimei ->
    if ekimei.kanji = shiten_namae then
        {namae = ekimei.kanji; saitan_kyori = 0.; temae_list = [shiten_namae]}
      else
        {namae = ekimei.kanji; saitan_kyori = infinity; temae_list = []}
  ) l
