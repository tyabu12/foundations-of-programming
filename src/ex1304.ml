(* 関数を2つ受け取り, 合成関数を返す *)
(* compose : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b *)
let compose f g =
  let h x = f (g x) in
  h

(* テスト *)
let () =
  let time2 x = 2 * x in
  let add3 x = 3 + x in
  assert ((compose time2 add3) 4 = 14);
  ()
