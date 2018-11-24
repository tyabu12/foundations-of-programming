open Ex1415

(* 自然数 n を受け取り, n の階乗を返す *)
(* fac : int -> int *)
let fac n = List.fold_right ( * ) (enumerate n) 1

(* テスト *)
let () =
  assert (fac 1 = 1);
  assert (fac 2 = 2);
  assert (fac 3 = 6);
  assert (fac 4 = 24);
  assert (fac 5 = 120);
  ()