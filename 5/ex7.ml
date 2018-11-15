open Printf

(* 目的: 身長 h と体重 w に応じて BMI を計算する *)
(* float -> float -> float *)
let bmi h w =
  w /. (h ** 2.)

(* 目的: 身長 h と体重 w に応じて体型を返す *)
(* float -> float -> string *)
let taikei h w =
  let t = bmi h w in
  if t < 18.5 then "やせ"
  else if t < 25. then "標準"
  else if t < 30. then "肥満"
  else "高度肥満"

let () =
  let println_bool x = print_endline (string_of_bool x) in
  println_bool ((taikei 1.58 55.) = "標準");
  println_bool ((taikei 1.68 55.) = "標準");
  println_bool ((taikei 1.78 55.) = "やせ");
  ()

