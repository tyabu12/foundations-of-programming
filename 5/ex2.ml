(* 目的: 受け取った時間 h に応じて午前か午後を計算する *)
(* int -> string *)
let jikan h =
  if h < 12 then "午前"
            else "午後"

(* テスト *)
let () =
  assert (jikan 0 = "午前");
  assert (jikan 6 = "午前");
  assert (jikan 12 = "午後");
  assert (jikan 18 = "午後");
  ()

