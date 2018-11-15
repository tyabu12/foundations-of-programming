open Printf

(* 目的: 名前 name と成績 score の組に応じて文字列を返す*)
(* string * string -> string *)
let seiseki name score =
  name ^ "さんの評価は" ^ score ^ "です"

let () =
  let println_bool x = print_endline (string_of_bool x) in
  (* テスト *)
  let test1 = seiseki "田中" "優" = "田中さんの評価は優です" in
  let test2 = seiseki "鈴木" "良" = "鈴木さんの評価は良です" in
  let test3 = seiseki "山本" "可" = "山本さんの評価は可です" in
  println_bool test1;
  println_bool test2;
  println_bool test3;
  ()

