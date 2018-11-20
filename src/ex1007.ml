open Ex0803

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

(* テスト *)
let () =
  let p1 = {name = "Mike"; height = 164; weight = 60; birth_month = 3; birth_day = 12; blood_type = "A"} in
  let p2 = {name = "Alice"; height = 172; weight = 66; birth_month = 4; birth_day = 2; blood_type = "B"} in
  let p3 = {name = "Kevin"; height = 182; weight = 76; birth_month = 8; birth_day = 21; blood_type = "A"} in
  let p4 = {name = "Joji"; height = 152; weight = 56; birth_month = 10; birth_day = 8; blood_type = "AB"} in
  assert (ketsueki_shukei [] = (0, 0, 0, 0));
  assert (ketsueki_shukei [p1] = (1, 0, 0, 0));
  assert (ketsueki_shukei [p1; p2] = (1, 1, 0, 0));
  assert (ketsueki_shukei [p1; p2; p3] = (2, 1, 0, 0));
  assert (ketsueki_shukei [p1; p2; p3; p4] = (2, 1, 0, 1));
  ()

