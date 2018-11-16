(* 目的: 名前 name と成績 score の組に応じて文字列を返す*)
(* string * string -> string *)
let seiseki name score =
  name ^ "さんの評価は" ^ score ^ "です"

(* テスト *)
let () =
  assert (seiseki "田中" "優" = "田中さんの評価は優です");
  assert (seiseki "鈴木" "良" = "鈴木さんの評価は良です");
  assert (seiseki "山本" "可" = "山本さんの評価は可です");
  ()

