type gakusei_t = {
  namae : string;   (* 名前 *)
  tensuu : int;     (* 点数 *)
  seiseki : string; (* 成績 *)
}

(* 目的: gakusei_t 型のリストを受け取り tensuu フィールド順に整列したリストを返す *)
(* gakusei_sort : gakusei_t list -> gakusei_t list *)
let rec gakusei_sort l =
  (* 目的: 点数昇順の gakusei_t 型のリスト l に昇順を崩さず gakusei_t 型の g を
           挿入したリストを返す *)
  (* insert : gakusei_t list -> gakusei_t -> gakusei_t list *)
  let rec insert l g = match l with
      [] -> [g]
    | {namae = n; tensuu = t; seiseki = s} as hd :: tl
       -> if t >= g.tensuu then g :: hd :: tl
                           else hd :: (insert tl g)
  in
  match l with
    [] -> []
  | hd :: tl -> insert (gakusei_sort tl) hd

(* テスト *)
let () =
  let g1 = {namae = "田中"; tensuu = 69; seiseki = "C"} in
  let g2 = {namae = "山本"; tensuu = 90; seiseki = "S"} in
  let g3 = {namae = "鈴木"; tensuu = 82; seiseki = "A"} in
  let g4 = {namae = "木村"; tensuu = 73; seiseki = "B"} in
  assert (gakusei_sort [] = []);
  assert (gakusei_sort [g1] = [g1]);
  assert (gakusei_sort [g1; g2] = [g1; g2]);
  assert (gakusei_sort [g1; g2; g3] = [g1; g3; g2]);
  assert (gakusei_sort [g1; g2; g3; g4] = [g1; g4; g3; g2]);
  ()

