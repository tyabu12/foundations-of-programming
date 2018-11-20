open Ex0803

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
  else if in_range m d  1 20  2 18 then "水瓶座"
  else if in_range m d  2 19  3 20 then "魚座座"
  else if in_range m d  3 21  4 19 then "牡羊座"
  else if in_range m d  4 20  5 20 then "牡牛座"
  else if in_range m d  5 21  6 21 then "双子座"
  else if in_range m d  6 22  7 22 then "蟹座"
  else if in_range m d  7 23  8 22 then "獅子座"
  else if in_range m d  8 23  9 22 then "乙女座"
  else if in_range m d  9 23 10 23 then "天秤座"
  else if in_range m d 10 24 11 22 then "蠍座"
  else if in_range m d 11 23 12 21 then "射手座"
  else if in_range m d 12 22  1 19 then "山羊座"
  else "不明"

(* 目的: person_t 型のリストから乙女座の人の名前からなるリストを返す *)
(* otomeza : person_t list -> string list *)
let rec otomeza l = match l with
    [] -> []
  | ({name = n; height = h; weight = w; birth_month = bm; birth_day = bd; blood_type = bt} as hd) :: tl
     -> if seiza bm bd = "乙女座" then hd :: otomeza tl
                                  else otomeza tl

(* テスト *)
let () =
  let p1 = {name = "Jack"; height = 172; weight = 66; birth_month = 4; birth_day = 2; blood_type = "B"} in
  let p2 = {name = "Kevin"; height = 182; weight = 76; birth_month = 8; birth_day = 25; blood_type = "A"} in
  let p3 = {name = "Taro"; height = 152; weight = 56; birth_month = 10; birth_day = 8; blood_type = "AB"} in
  let p4 = {name = "Kate"; height = 164; weight = 60; birth_month = 9; birth_day = 12; blood_type = "A"} in
  assert (otomeza [] = []);
  assert (otomeza [p2] = [p2]);
  assert (otomeza [p1; p2] = [p2]);
  assert (otomeza [p1; p2; p3] = [p2]);
  assert (otomeza [p1; p2; p3; p4] = [p2; p4]);
  ()

