(* 目的: 配列を与えられたら, そこにフィボナッチ数を順に入れた配列を返す *)
(* fib_array : int array -> int array *)
let fib_array arr =
  let len = Array.length arr in
  let rec aux n =
    if n >= len then arr
    else (
      arr.(n) <- (
        if n = 0 then 0
        else if n = 1 then 1
        else arr.(n - 2) + arr.(n - 1)
      );
      aux (n + 1)
    )
  in aux 0

(* テスト *)
let () =
  assert (fib_array [||] = [||]);
  assert (fib_array [|0; 0; 0; 0; 0; 0; 0;  0;  0;  0|] =
                    [|0; 1; 1; 2; 3; 5; 8; 13; 21; 34|]);
  ()