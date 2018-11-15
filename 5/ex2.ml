open Printf

(* 目的: 受け取った時間 h に応じて午前か午後を計算する *)
(* int -> string *)
let jikan h =
  if h < 12 then "午前"
            else "午後"

let () =
  let println_bool x = print_endline (string_of_bool x) in
  (* テスト *)
  let test1 = jikan 0 = "午前" in
  let test2 = jikan 6 = "午前" in
  let test3 = jikan 12 = "午後" in
  let test4 = jikan 18 = "午後" in
  println_bool test1;
  println_bool test2;
  println_bool test3;
  println_bool test4;
  ()

