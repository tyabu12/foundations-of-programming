(* 目的: item の値段を調べる *)
(* price : string -> (string * int) list -> int option *)
let rec price item yaoya_list = match yaoya_list with
  | [] -> None
  | (yasai, nedan) :: rest ->
    if item = yasai then Some nedan
                    else price item rest

(* 目的: 野菜のリスト yasai_lst と八百屋のリスト yaoya_lst を受け取り,
        野菜のリストのうち八百屋に置いていない野菜の数を返す *)
(* count_urikire_yasai : string list -> (string * int) list -> int *)
let count_urikire_yasai yasai_lst yaoya_lst =
  List.fold_left (fun cnt yasai ->
    cnt + match (price yasai yaoya_lst) with
          | None -> 0
          | Some _ -> 1
  ) 0 yasai_lst

(* テスト *)
let () =
  assert (count_urikire_yasai [] [] = 0);
  assert (count_urikire_yasai ["トマト"] [] = 0);
  assert (count_urikire_yasai ["トマト"] [("トマト", 300)] = 1);
  let yaoya_lst = [("トマト", 300); ("たまねぎ", 200); ("にんじん", 150); ("ほうれん草", 200)] in
  assert (count_urikire_yasai ["トマト"] yaoya_lst = 1);
  assert (count_urikire_yasai ["たまねぎ"; "トマト"; "じゃがいも"; "カレー粉"; "すじ肉"] yaoya_lst = 2);
  ()
