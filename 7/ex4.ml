open Printf

(* 目的: 平面座標 xy1, xy2 の中点座標を返す *)
(* chuten: float * float -> float * float -> float * float *)
let chuten xy1 xy2 = match xy1, xy2 with
  (x1, y1), (x2, y2) -> ((x1 +. x2) /. 2., (y1 +. y2) /. 2.)

let () =
  let println_bool x = print_endline (string_of_bool x) in
  (* テスト *)
  let test1 = chuten (1., 2.) (1., -2.) = (1., 0.) in
  let test2 = chuten (3., 5.) (4., 8.) = (3.5, 6.5) in
  let test3 = chuten (3., -8.) (-3., -8.) = (0., -8.) in
  println_bool test1;
  println_bool test2;
  println_bool test3;
  ()

