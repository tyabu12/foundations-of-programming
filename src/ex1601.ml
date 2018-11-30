(* 目的: 整数のリストを受け取り, それまでの数の合計からなるリストを返す *)
(* sum_list : int list -> int list *)
let sum_list l =
  (* 目的: 整数のリストを受け取り, それまでの数の合計からなるリストを返す *)
  (* ここで acc は これまでの整数の合計 *)
  (* aux : int list -> int -> int list *)
  let rec aux l' acc = match l' with
    | [] -> []
    | hd :: tl ->
      let acc' = hd + acc in
      acc' :: (aux tl acc')
  in
  aux l 0

(* テスト *)
let () =
  assert (sum_list [] = []);
  assert (sum_list [3; 2; 1; 4] = [3; 5; 6; 10]);
  ()
