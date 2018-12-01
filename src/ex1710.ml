type ekikan_tree_t =
  | Empty
  | Node of ekikan_tree_t * string * (string * float) list * ekikan_tree_t

(* 目的: 木の print (デバッグ用) *)
(* print_tree : ekikan_tree_t -> unit *)
let print_tree t =
  let rec aux tree = match tree with
    | Empty -> print_string "Empty"
    | Node (l, s, lst, r) ->
      print_char '(';
      aux l;
      print_string ("," ^ s ^ ",");
      print_char '[';
        List.iter (fun (s, d) ->
          print_string ("(" ^ s ^ "," ^ string_of_float d ^ ");")
        ) lst;
      print_string "],";
      aux r;
      print_char ')'
  in
  aux t;
  print_newline ()

(* テスト *)
let () =
  let _ = Empty in
  let _ = Node (Empty, "茗荷谷", [("新大塚", 1.2); ("後楽園", 1.8)], Empty) in
  ()
