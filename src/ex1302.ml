open Ex0803

(* 目的: person_t 型のリストを受け取り, その中の人名のリストを返す *)
(* person_namae : person_t list -> string list *)
let rec person_namae person_list =
  let f person = person.name in
  List.map f person_list

(* テスト *)
let () =
  let p1 = {name = "Mike"; height = 164; weight = 60; birth_month = 3; birth_day = 12; blood_type = "A"} in
  let p2 = {name = "Alice"; height = 172; weight = 66; birth_month = 4; birth_day = 2; blood_type = "B"} in
  let p3 = {name = "Kevin"; height = 182; weight = 76; birth_month = 8; birth_day = 21; blood_type = "A"} in
  assert (person_namae [p1] = [p1.name]);
  assert (person_namae [p1; p2] = [p1.name; p2.name]);
  assert (person_namae [p1; p2; p3] = [p1.name; p2.name; p3.name]);
  ()
