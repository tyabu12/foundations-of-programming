open Gakusei

(* 目的: 学生リスト lst のうち成績が A の人の数を返す *)
(* count_A : gakusei_t list -> int *)
let count_A lst =
  let is_A gakusei = gakusei.seiseki = "A" in
  List.length (List.filter is_A lst)

(* テスト *)
let () =
  let g = [
    {namae = "田中"; tensuu = 69; seiseki = "C"};
    {namae = "山本"; tensuu = 90; seiseki = "B"};
    {namae = "鈴木"; tensuu = 82; seiseki = "A"};
    {namae = "木村"; tensuu = 73; seiseki = "A"};
  ] in
  assert (count_A g = 2);
  ()
