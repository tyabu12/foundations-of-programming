open Ex0803

(* 目的: person_t 型のリスト l を受け取り,
        その中から最初のA型の人々のレコードをオプション型で返す *)
(* first_A : person_t list -> person_t option *)
let first_A l =
  match List.filter (fun p -> p.blood_type = "A") l with
  | [] -> None
  | hd :: _ -> Some hd

(* テスト *)
let () =
  let p1 = {name = "Alice"; height = 172; weight = 66; birth_month = 4; birth_day = 2; blood_type = "B"} in
  let p2 = {name = "Kevin"; height = 182; weight = 76; birth_month = 8; birth_day = 21; blood_type = "A"} in
  let p3 = {name = "Joji"; height = 152; weight = 56; birth_month = 10; birth_day = 8; blood_type = "AB"} in
  let p4 = {name = "Mike"; height = 164; weight = 60; birth_month = 3; birth_day = 12; blood_type = "A"} in
  assert (first_A [] = None);
  assert (first_A [p1] = None);
  assert (first_A [p1; p2] = Some p2);
  assert (first_A [p1; p2; p3; p4] = Some p2);
  assert (first_A [p4; p3; p2; p1] = Some p4);
  ()
