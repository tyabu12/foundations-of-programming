(* 目的: 平面座標 xy1, xy2 の中点座標を返す *)
(* chuten: float * float -> float * float -> float * float *)
let chuten xy1 xy2 = match xy1, xy2 with
  (x1, y1), (x2, y2) -> ((x1 +. x2) /. 2., (y1 +. y2) /. 2.)

(* テスト *)
let () =
  assert (chuten (1., 2.) (1., -2.) = (1., 0.));
  assert (chuten (3., 5.) (4., 8.) = (3.5, 6.5));
  assert (chuten (3., -8.) (-3., -8.) = (0., -8.));
  ()

