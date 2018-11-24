open Metro
open Ex1201 (* eki_t *)

(* 目的: ekimei_t list を受け取り, その駅名に応じた eki_t list を返す *)
(* make_eki_list :ekimei_t list -> eki_t list *)
let rec make_eki_list ekimei_list =
  List.map (fun ekimei -> {namae = ekimei.kanji; saitan_kyori = infinity; temae_list = []}) ekimei_list

(* 目的: ekimei_t list と漢字の駅名 shiten_namae を受け取り,
 *       その駅名に応じた eki_t list を返す *)
(* shokika : ekimei_t list -> string -> eki_t list *)
let rec shokika ekimei_list shiten_namae =
  List.map (fun ekimei ->
    if ekimei.kanji = shiten_namae then
        {namae = ekimei.kanji; saitan_kyori = 0.; temae_list = [shiten_namae]}
      else
        {namae = ekimei.kanji; saitan_kyori = infinity; temae_list = []}
  ) ekimei_list

(* 目的: eki_t list と漢字の駅名 shiten_namae を受け取り,
 *       その駅名に応じた eki_t list を返す *)
(* shokika : eki_t list -> string -> eki_t list *)
let rec shokika eki_list shiten_namae =
  List.map (fun eki ->
    if eki.namae = shiten_namae then
        {namae = eki.namae; saitan_kyori = 0.; temae_list = [shiten_namae]}
      else
        {namae = eki.namae; saitan_kyori = infinity; temae_list = []}
  ) eki_list

(* テスト *)
let () =
  let ekimei1 = {kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; shozoku="千代田線"} in
  let ekimei2 = {kanji="湯島"; kana="ゆしま"; romaji="yushima"; shozoku="千代田線"} in
  let ekimei3 = {kanji="田原町"; kana="たわらまち"; romaji="tawaramachi"; shozoku="銀座線"} in
  let eki ekimei =
    {namae = ekimei.kanji; saitan_kyori = infinity; temae_list = []}
  in
  let shiten_eki ekimei =
    {namae = ekimei.kanji; saitan_kyori = 0.; temae_list = [ekimei.kanji]}
  in
  let eki1 = eki ekimei1 and eki2 = eki ekimei2 and eki3 = eki ekimei3 in
  (* make_eki_list *)
  assert (make_eki_list [] = []);
  assert (make_eki_list [ekimei1] = [eki1]);
  assert (make_eki_list [ekimei1; ekimei2] = [eki1; eki2]);
  assert (make_eki_list [ekimei1; ekimei2; ekimei3] = [eki1; eki2; eki3]);
  (* shokika *)
  assert (shokika [] "" = []);
  assert (shokika [] "湯島" = []);
  assert (shokika [eki ekimei1] "へのへの" = [eki ekimei1]);
  assert (shokika [eki ekimei1] ekimei1.kanji = [shiten_eki ekimei1]);
  assert (shokika [eki ekimei1; eki ekimei2] ekimei2.kanji = [eki ekimei1; shiten_eki ekimei2]);
  assert (shokika [eki ekimei1; eki ekimei2] ekimei2.kanji = [eki ekimei1; shiten_eki ekimei2]);
  assert (shokika [eki ekimei1; eki ekimei2; eki ekimei3] ekimei2.kanji= [eki ekimei1; shiten_eki ekimei2; eki ekimei3]);
  ()
