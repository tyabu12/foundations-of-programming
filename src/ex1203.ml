open Metro
open Ex1201 (* eki_t *)

(* 目的: eki_t list と漢字の駅名 shiten_namae を受け取り,
 *       その駅名に応じた eki_t list を返す *)
(* shokika : ekimei_t list -> string -> eki_t list *)
let rec shokika eki_list shiten_namae = match eki_list with
  | [] -> []
  | hd :: tl ->
      (if hd.namae = shiten_namae then
         {namae = hd.namae; saitan_kyori = 0.; temae_list = [shiten_namae]}
       else
         hd
      ) :: (shokika tl shiten_namae)

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
  assert (shokika [] "" = []);
  assert (shokika [] "湯島" = []);
  assert (shokika [eki ekimei1] "へのへの" = [eki ekimei1]);
  assert (shokika [eki ekimei1] ekimei1.kanji = [shiten_eki ekimei1]);
  assert (shokika [eki ekimei1; eki ekimei2] ekimei2.kanji = [eki ekimei1; shiten_eki ekimei2]);
  assert (shokika [eki ekimei1; eki ekimei2] ekimei2.kanji = [eki ekimei1; shiten_eki ekimei2]);
  assert (shokika [eki ekimei1; eki ekimei2; eki ekimei3] ekimei2.kanji= [eki ekimei1; shiten_eki ekimei2; eki ekimei3]);
  ()

