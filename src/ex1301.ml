open Ex0803

(* 目的: person_t 型のリスト受け取り, 指定された血液型 bt の人の数を返す *)
(* count_ketsueki : person_t list -> string -> int *)
let rec count_ketsueki person_list bt = match person_list with
  | [] -> 0
  | hd :: tl ->
    let cnt = count_ketsueki tl bt in
    if hd.blood_type = bt then 1 + cnt
                          else cnt

(* テスト *)
let () =
  let p1 = {name = "Alice"; height = 172; weight = 66; birth_month = 4; birth_day = 2; blood_type = "B"} in
  let p2 = {name = "Kevin"; height = 182; weight = 76; birth_month = 8; birth_day = 21; blood_type = "A"} in
  let p3 = {name = "Joji"; height = 152; weight = 56; birth_month = 10; birth_day = 8; blood_type = "AB"} in
  let p4 = {name = "Mike"; height = 164; weight = 60; birth_month = 3; birth_day = 12; blood_type = "A"} in
  assert (count_ketsueki [] "A" = 0);
  assert (count_ketsueki [p1] "A" = 0);
  assert (count_ketsueki [p1] "B" = 1);
  assert (count_ketsueki [p2] "A" = 1);
  assert (count_ketsueki [p1; p2] "A" = 1);
  assert (count_ketsueki [p1; p2] "B" = 1);
  assert (count_ketsueki [p1; p2; p3] "A" = 1);
  assert (count_ketsueki [p1; p2; p3; p4] "A" = 2);
  ()
