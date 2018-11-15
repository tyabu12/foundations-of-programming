open Printf

(* 目的: 平面座標 xy に応じた x 軸に対称な点の座標を返す *)
(* taisho_x: float * float -> float * float *)
let taisho_x xy = match xy with
  (x, y) -> (x, -. y)

let () =
  let println_bool x = print_endline (string_of_bool x) in
  (* テスト *)
  let test1 = taisho_x (1., 2.) = (1., -2.) in
  let test2 = taisho_x (4., -8.) = (4., 8.) in
  let test3 = taisho_x (-3., -8.) = (-3., 8.) in
  println_bool test1;
  println_bool test2;
  println_bool test3;
  ()

