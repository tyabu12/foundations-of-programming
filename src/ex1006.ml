open Gakusei

(* 目的: gakusei_t 型のリストを受け取り tensuu フィールドのレコード返す *)
(* gakusei_max : gakusei_t list -> gakusei_t*)
let rec gakusei_max l =
  match l with
    [] -> {namae = ""; tensuu = -max_int; seiseki = ""} (* tensuu は 0 でもいいかも *)
  | hd :: tl ->
      let g_max = gakusei_max tl in
      if g_max.tensuu > hd.tensuu then g_max else hd

(* テスト *)
let () =
  let g1 = {namae = "田中"; tensuu = 69; seiseki = "C"} in
  let g2 = {namae = "山本"; tensuu = 90; seiseki = "S"} in
  let g3 = {namae = "鈴木"; tensuu = 82; seiseki = "A"} in
  let g4 = {namae = "木村"; tensuu = 73; seiseki = "B"} in
  assert ((gakusei_max []).tensuu = -max_int);
  assert (gakusei_max [g1] = g1);
  assert (gakusei_max [g1; g2] = g2);
  assert (gakusei_max [g1; g2; g3] = g2);
  assert (gakusei_max [g1; g2; g3; g4] = g2);
  ()

