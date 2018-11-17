type person_t = {
  name : string;       (* 名前 *)
  height : int;        (* 身長(m) *)
  weight : int;        (* 体重(kg) *)
  birth_month : int;   (* 誕生月 *)
  birth_day : int;     (* 誕生日 *)
  blood_type : string; (* 血液型 *)
}

(* 目的: person_t 型のリストを受け取り name フィールド順のリストを返す *)
(* person_sort : person_t list -> person_t list *)
let rec person_sort l =
  (* 目的: 名前昇順の person_t 型のリスト l に昇順を崩さず person_t 型の p を
   *       挿入したリストを返す *)
  (* insert : person_t list -> person_t -> person_t list *)
  let rec insert l p = match l with
      [] -> [p]
    | {name = n;
       height = h;
       weight = w;
       birth_month = bm;
       birth_day = bd;
       blood_type = bt}
      as hd :: tl
       -> if n >= p.name then p :: hd :: tl
                         else hd :: (insert tl p)
  in
  match l with
    [] -> []
  | hd :: tl -> insert (person_sort tl) hd

(* テスト *)
let () =
  let p1 = {name = "Mike"; height = 164; weight = 60; birth_month = 3; birth_day = 12; blood_type = "A"} in
  let p2 = {name = "Alice"; height = 172; weight = 66; birth_month = 4; birth_day = 2; blood_type = "B"} in
  let p3 = {name = "Kevin"; height = 182; weight = 76; birth_month = 8; birth_day = 21; blood_type = "A"} in
  let p4 = {name = "Joji"; height = 152; weight = 56; birth_month = 10; birth_day = 8; blood_type = "AB"} in
  assert (person_sort [] = []);
  assert (person_sort [p1] = [p1]);
  assert (person_sort [p1; p2] = [p2; p1]);
  assert (person_sort [p1; p2; p3] = [p2; p3; p1]);
  assert (person_sort [p1; p2; p3; p4] = [p2; p4; p3; p1]);
  ()

