(* 月 *)
type year_t =
  | January of int
  | February of int
  | March of int
  | April of int
  | May of int
  | June of int
  | July of int
  | August of int
  | September of int
  | October of int
  | November of int
  | Decenber of int

(* テスト *)
let () =
  let _ = January 1 in
  let _ = July 8 in
  ()
