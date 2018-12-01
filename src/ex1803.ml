open Ex1710
open CheckRaise

(* 目的: 「駅名」 s と「駅名と距離のリスト」 l を受け取り, その駅までの距離を返す *)
(* assoc : 'a -> ('a * 'b) list -> 'b*)
let rec assoc s l =
  let l' = List.filter (fun (namae, d) -> namae = s) l in
  match l' with
  | [] -> raise Not_found
  | (_, d) :: _ -> d

(* テスト *)
let () =
  assert (check_raise (fun () -> assoc "" []) Not_found);
  assert (assoc "後楽園" [("新大塚", 1.2); ("後楽園", 1.8)] = 1.8);
  assert (check_raise (fun () -> assoc "池袋" [("新大塚", 1.2); ("後楽園", 1.8)]) Not_found);
  ()
