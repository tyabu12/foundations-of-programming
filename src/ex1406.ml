open Gakusei

(* 目的: 学生リスト lst のうち成績が seiseki0 の人の数を返す *)
(* count : gakuseki_t list -> string -> int *)
let count lst seiseki0 =
  let f g = g.seiseki = seiseki0 in
  List.length (List.filter f lst)

(* テスト *)
let () =
  let g = [
    {namae = "田中"; tensuu = 69; seiseki = "C"};
    {namae = "山本"; tensuu = 90; seiseki = "S"};
    {namae = "鈴木"; tensuu = 82; seiseki = "A"};
    {namae = "木村"; tensuu = 73; seiseki = "B"};
    {namae = "吉田"; tensuu = 78; seiseki = "B"};
  ] in
  assert (count [] "S" = 0);
  assert (count g "S" = 1);
  assert (count g "A" = 1);
  assert (count g "B" = 2);
  assert (count g "C" = 1);
  ()
