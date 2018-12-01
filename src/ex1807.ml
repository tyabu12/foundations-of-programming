open Metro
open Ex1806
open CheckRaise

(* 目的: ローマ字駅名 romaji_namae と駅名リスト ekimei_list を受け取り,
        その駅の漢字表記を返す *)
(* romaji_to_kanji : string -> ekimei_t list -> string *)
let rec romaji_to_kanji romaji_namae ekimei_list = match ekimei_list with
  | [] -> raise (No_such_station romaji_namae)
  | hd :: tl -> if hd.romaji = romaji_namae then hd.kanji
                                            else romaji_to_kanji romaji_namae tl

(* テスト *)
let () =
  let ekimei_list = global_ekimei_list in
  assert (check_raise (fun () -> romaji_to_kanji "" ekimei_list = "") (No_such_station ""));
  assert (romaji_to_kanji "myogadani" ekimei_list = "茗荷谷");
  assert (romaji_to_kanji "shinotsuka" ekimei_list = "新大塚");
  assert (check_raise (fun () -> romaji_to_kanji "fukui" ekimei_list) (No_such_station "fukui"));
  ()

