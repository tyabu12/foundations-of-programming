open Ex2001
open Ex2003
open CheckRaise

(* 目的: rb_tree_t 型の赤黒木と key を受け取り, key に一致する value を返す *)
(* search : ('a 'b) rb_tree_t -> 'b *)
let rec search tree key = match tree with
  | Empty -> raise Not_found
  | Node (left, k, value, _, right) ->
    if k = key then value
    else if key < k then search left key
    else search right key

(* テスト *)
let () =
  assert (check_raise (fun () -> search Empty "a") Not_found);
  let tree = List.fold_right
    (fun s tr -> insert tr s (s ^ "v"))
    ["a"; "b"; "c"; "d"; "e"; "f"; "g"]
    Empty
  in
  assert (search tree "b" = "bv");
  assert (check_raise (fun () -> search tree "i") Not_found);
