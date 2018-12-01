(* 目的: 最小値の候補 m と 残りのリスト lst を受け取り, 最小値を返す *)
(* minimum : int -> int list -> int *)
let minimum m lst =
  List.fold_left min m lst

let () =
  assert (minimum 3 [] = 3);
  assert (minimum 0 [2; 3; 4] = 0);
  assert (minimum 3 [2; 3; 4] = 2);
  ()
