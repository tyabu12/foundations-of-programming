open Ex1007

(* 目的: person_t 型のリストを受け取り最も血液型を返す *)
(* saita_ketsueki : person_t list -> string *)
let rec saita_ketsueki l = match ketsueki_shukei l with
    (0, 0, 0, 0) -> ""
  | (a, b, o, ab) -> if a >= b && a >= o && a >= ab then "A"
                     else if b >= o && b >= ab then "B"
                     else if o >= ab then "O"
                     else "AB"

(* テスト *)
let () =
  let p1 = {name = "Mike"; height = 164; weight = 60; birth_month = 3; birth_day = 12; blood_type = "A"} in
  let p2 = {name = "Alice"; height = 172; weight = 66; birth_month = 4; birth_day = 2; blood_type = "B"} in
  let p3 = {name = "Kevin"; height = 182; weight = 76; birth_month = 8; birth_day = 21; blood_type = "O"} in
  let p4 = {name = "Joji"; height = 152; weight = 56; birth_month = 10; birth_day = 8; blood_type = "AB"} in
  assert (saita_ketsueki [] = "");
  assert (saita_ketsueki [p1] = "A");
  assert (saita_ketsueki [p1; p1; p2] = "A");
  assert (saita_ketsueki [p1; p1; p2; p2; p2;] = "B");
  assert (saita_ketsueki [p1; p1; p2; p2; p2; p3; p4] = "B");
  assert (saita_ketsueki [p1; p1; p2; p2; p3; p3; p3] = "O");
  ()

