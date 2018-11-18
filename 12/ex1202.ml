open Metro

type eki_t = {
  namae : string;           (* 漢字の駅名 *)
  saitan_kyori : float;     (* 最短距離 *)
  temae_list : string list; (* 漢字の駅名リスト *)
}

(* 目的: ekimei_t list を受け取り, その駅名に応じた eki_t list を返す *)
(* make_eki_list :ekimei_t list -> eki_t list *)
let rec make_eki_list ekimei_list = match ekimei_list with
  | [] -> []
  | {kanji=kanji_namae} :: tl ->
      {namae = kanji_namae; saitan_kyori = infinity; temae_list = []}
        :: make_eki_list tl

(* テスト *)
let () =
  let ekimei1 = {kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; shozoku="千代田線"} in
  let ekimei2 = {kanji="湯島"; kana="ゆしま"; romaji="yushima"; shozoku="千代田線"} in
  let ekimei3 = {kanji="田原町"; kana="たわらまち"; romaji="tawaramachi"; shozoku="銀座線"} in
  let eki ekimei =
    {namae = ekimei.kanji; saitan_kyori = infinity; temae_list = []}
  in
  let eki1 = eki ekimei1 and eki2 = eki ekimei2 and eki3 = eki ekimei3 in
  assert (make_eki_list [] = []);
  assert (make_eki_list [ekimei1] = [eki1]);
  assert (make_eki_list [ekimei1; ekimei2] = [eki1; eki2]);
  assert (make_eki_list [ekimei1; ekimei2; ekimei3] = [eki1; eki2; eki3]);
  ()
