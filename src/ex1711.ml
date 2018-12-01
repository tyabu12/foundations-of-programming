open Ex1710

(* 目的: 「駅名」 s と「駅名と距離のリスト」 l を受け取り, その駅までの距離を返す *)
(* assoc : string -> (string * float) list -> float *)
let rec assoc s l =
  let l' = List.filter (fun (namae, d) -> namae = s) l in
  match l' with
  | [] -> infinity
  | (_, d) :: _ -> d

(* テスト *)
let () =
  assert (assoc "" [] = infinity);
  assert (assoc "後楽園" [("新大塚", 1.2); ("後楽園", 1.8)] = 1.8);
  assert (assoc "池袋" [("新大塚", 1.2); ("後楽園", 1.8)] = infinity);
  ()
