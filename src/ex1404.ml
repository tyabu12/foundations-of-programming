open Gakusei

(* gakusei_t 型のリストを受け取り, 全員の特典の合計を返す *)
(* gakusei_sum : gakusei_t list -> int *)
let gakusei_sum l =
  let f g tensuu = g.tensuu + tensuu in
  List.fold_right f l 0

(* テスト *)
let () =
  let g1 = {namae = "田中"; tensuu = 69; seiseki = "C"} in
  let g2 = {namae = "山本"; tensuu = 90; seiseki = "S"} in
  let g3 = {namae = "鈴木"; tensuu = 82; seiseki = "A"} in
  let g4 = {namae = "木村"; tensuu = 73; seiseki = "B"} in
  assert (gakusei_sum [] = 0);
  assert (gakusei_sum [g1] = 69);
  assert (gakusei_sum [g1; g2; g3; g4] = 314);
  ()
