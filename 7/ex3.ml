open Printf

(* 目的: 平面座標 xy に応じた x 軸に対称な点の座標を返す *)
(* taisho_x: float * float -> float * float *)
let taisho_x xy = match xy with
  (x, y) -> (x, -. y)

(* テスト *)
let () =
  assert (taisho_x (1., 2.) = (1., -2.));
  assert (taisho_x (4., -8.) = (4., 8.));
  assert (taisho_x (-3., -8.) = (-3., 8.));
  ()

