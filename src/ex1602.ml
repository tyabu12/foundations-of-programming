(* 目的: 関数 f, 初期値 init, リスト lst を受け取り, 左畳み込みを行う *)
(* fold_left : (a' -> b' -> a') -> a' -> b' list -> a' *)
let fold_left f init lst =
  let rec aux acc l = match l with
    | [] -> acc
    | hd :: tl -> aux (f acc hd) tl
  in
  aux init lst

(* テスト *)
let () =
  assert (fold_left (+) 0 [] = 0);
  assert (fold_left (+) 0 [1; 2; 3; 4; 5] = 15);
  assert (fold_left ( * ) 0 [1; 2; 3; 4; 5] = 0);
  assert (fold_left ( * ) 1 [1; 2; 3; 4; 5] = 120);
  ()
