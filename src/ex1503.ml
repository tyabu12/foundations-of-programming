(* 目的: 2以上 n 以下の自然数のリスト l を受け取り, そのうちの素数のリストを返す *)
(* sieve : int -> int list -> int list *)
let rec sieve l =
  (* print_list l; *)
  match l with
  | [] -> []
  | hd :: tl ->
    hd :: (sieve (List.filter (fun x -> x mod hd <> 0) tl))

(* 目的: 自然数 n を受け取り, n 以下の素数のリストを返す *)
(* prime : int -> int list *)
let prime n =
  let rec enumurate i =
    if i > n then []
             else i :: enumurate (i + 1)
  in
  sieve (enumurate 2)

(* テスト *)
let () =
  assert (prime 2 = [2]);
  assert (prime 10 = [2; 3; 5; 7]);
  assert (prime 20 = [2; 3; 5; 7; 11; 13; 17; 19]);
  ()
