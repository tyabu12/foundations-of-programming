type person_t = {
  name : string;        (* 名前 *)
  height : int;        (* 身長(m) *)
  weight : int;        (* 体重(kg) *)
  birth_month : int;   (* 誕生月 *)
  birth_day : int;     (* 誕生日 *)
  blood_type : string; (* 血液型 *)
}

(* 目的: person_t 型のリストから血液型がA型の人の数を返す *)
(* count_ketsueki_A : person_t list -> int *)
let rec count_ketsueki_A l = match l with
    [] -> 0
  | {name = n; height = h; weight = w; birth_month = bm; birth_day = bd; blood_type = bt} :: tl
     -> if bt = "A" then 1 + count_ketsueki_A tl
                    else count_ketsueki_A tl

(* テスト *)
let () =
  let p1 = {name = "Alice"; height = 172; weight = 66; birth_month = 4; birth_day = 2; blood_type = "B"} in
  let p2 = {name = "Kevin"; height = 182; weight = 76; birth_month = 8; birth_day = 21; blood_type = "A"} in
  let p3 = {name = "Joji"; height = 152; weight = 56; birth_month = 10; birth_day = 8; blood_type = "AB"} in
  let p4 = {name = "Mike"; height = 164; weight = 60; birth_month = 3; birth_day = 12; blood_type = "A"} in
  assert (count_ketsueki_A [] = 0);
  assert (count_ketsueki_A [p2] = 1);
  assert (count_ketsueki_A [p1; p2] = 1);
  assert (count_ketsueki_A [p1; p2; p3] = 1);
  assert (count_ketsueki_A [p1; p2; p3; p4] = 2);
  ()

