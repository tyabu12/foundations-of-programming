type person_t = {
  name : string;       (* 名前 *)
  height : int;        (* 身長(m) *)
  weight : int;        (* 体重(kg) *)
  birth_month : int;   (* 誕生月 *)
  birth_day : int;     (* 誕生日 *)
  blood_type : string; (* 血液型 *)
}

(* 目的: person_t 型のリストを受け取り各血液型の人が何人いるかを組にしてを返す *)
(* ketsueki_shukei : person_t list -> person_t list *)
let rec ketsueki_shukei l = match l with
    [] -> (0, 0, 0, 0)
  | {name = n; height = h; weight = w; birth_month = bm; birth_day = bd; blood_type = bt} :: tl
     -> match ketsueki_shukei tl with
        (a, b, o, ab) -> match bt with
                           "A" -> (a + 1, b, o, ab)
                         | "B" -> (a, b + 1, o, ab)
                         | "O" -> (a, b, o + 1, ab)
                         | "AB" -> (a, b, o, ab + 1)
                         | _ -> (a, b, o, ab)

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

