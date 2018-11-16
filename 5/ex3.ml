(* 目的: m/d が sm/sd ~ em/ed の期間内かを計算する *)
(* int -> int -> int -> int -> int -> int -> bool *)
let in_range m d sm sd em ed =
  if sm <> em then
    (if sm < em then sm <= m && m <= em
                else sm <= m || m <= em)
    && (m <> sm || sd <= d) && (m <> em || d <= ed)
  else
    if sd <= ed then sd <= d && d <= ed
                else sd <= d || d <= ed

(* 目的: 受け取った月 m と日 d に応じて星座を計算する *)
(* int -> int -> string *)
let seiza m d =
  if m < 1 || m > 12 || d < 1 || d > 31 then "無効な日付"
  else if in_range m d  1 20  2 18 then "みずがめ座"
  else if in_range m d  2 19  3 20 then "うお座"
  else if in_range m d  3 21  4 19 then "おひつじ座"
  else if in_range m d  4 20  5 20 then "おうし座"
  else if in_range m d  5 21  6 21 then "ふたご座"
  else if in_range m d  6 22  7 22 then "かに座"
  else if in_range m d  7 23  8 22 then "しし座"
  else if in_range m d  8 23  9 22 then "おとめ座"
  else if in_range m d  9 23 10 23 then "てんびん座"
  else if in_range m d 10 24 11 22 then "さそり座"
  else if in_range m d 11 23 12 21 then "いて座"
  else if in_range m d 12 22  1 19 then "やぎ座"
  else "不明"

(* テスト *)
let () =
  assert (seiza 1 1 = "やぎ座");
  assert (seiza 3 6 = "うお座");
  assert (seiza 12 3 = "いて座");
  assert (seiza 8 18 = "しし座");
  ()

