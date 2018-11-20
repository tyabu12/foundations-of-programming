open Ex0404

(* 目的: 身長 h と体重 w に応じて体型を返す *)
(* float -> float -> string *)
let taikei h w =
  let t = bmi h w in
  if t < 18.5 then "やせ"
  else if t < 25. then "標準"
  else if t < 30. then "肥満"
  else "高度肥満"

(* テスト *)
let () =
  assert ((taikei 1.58 55.) = "標準");
  assert ((taikei 1.68 55.) = "標準");
  assert ((taikei 1.78 55.) = "やせ");
  ()

