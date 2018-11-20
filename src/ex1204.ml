open Metro

(* 目的: ekimei_t 型のリストを受け取り, ひらがな昇順に整列し駅の重複を取り除いたリストを返す *)
(* seiretsu : ekimei_t list -> ekimei_t list *)
let rec seiretsu ekimei_list =
  (* 目的: ひらがな昇順の整数リスト l に昇順を崩さず ekimei_t を挿入したリストを返す *)
  (* insert : ekimei_t list -> ekimei_t -> ekimei_t list *)
  let rec insert l ekimei = match l with
    | [] -> [ekimei]
    | hd :: tl ->
        if ekimei.kana < hd.kana then
          ekimei :: l
        else if ekimei.kana > hd.kana then
          hd :: (insert tl ekimei)
        else (* 重複 *)
          l
  in
  match ekimei_list with
  | [] -> []
  | hd :: tl -> insert (seiretsu tl) hd

(* テスト *)
let () =
  let ekimei1 = {kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; shozoku="千代田線"} in
  let ekimei2 = {kanji="渋谷"; kana="しぶや"; romaji="shibuya"; shozoku="半蔵門線"} in
  let ekimei3 = {kanji="六本木"; kana="ろっぽんぎ"; romaji="roppongi"; shozoku="日比谷線"} in
  assert (seiretsu [] = []);
  assert (seiretsu [ekimei1; ekimei2] = [ekimei2; ekimei1]);
  assert (seiretsu [ekimei1; ekimei2; ekimei3] = [ekimei2; ekimei1; ekimei3]);
  assert (seiretsu [ekimei1; ekimei2; ekimei1] = [ekimei2; ekimei1]);
  assert (seiretsu [ekimei1; ekimei2; ekimei1; ekimei2] = [ekimei2; ekimei1]);
  ()
