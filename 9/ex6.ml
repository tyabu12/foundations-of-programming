(* 目的: 文字列リストの全ての要素を前から順に全てくっつけた文字列を返す *)
(* concat : string list -> string *)
let rec concat l = match l with
    [] -> ""
  | hd :: tl -> hd ^ (concat tl)

(* テスト *)
let () =
  assert (concat [] = "");
  assert (concat [""] = "");
  assert (concat [""; ""] = "");
  assert (concat ["a"] = "a");
  assert (concat ["a"; "b"] = "ab");
  assert (concat ["a"; "bc"; "d"] = "abcd");
  assert (concat ["春"; "夏"; "秋"; "冬"] = "春夏秋冬");
  assert (concat ["ま"; "つ"; "たけ"; "食べ"; "たい"; ""] = "まつたけ食べたい");
  ()

