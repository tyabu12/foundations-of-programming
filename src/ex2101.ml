open Ex1201

(* 目的: eki_t 型の値を受け取り, きれいに表示する *)
(* print_eki : eki_t -> unit *)
let print_eki eki =
  (print_string eki.namae;
   print_string "駅の最短距離は";
   print_float eki.saitan_kyori;
   print_string "km です。";
   print_string "手前の駅は";
   print_string (List.fold_left (fun acc s -> acc ^ ", " ^ s) "" eki.temae_list);
   print_string "です。";
   print_newline ())

(* テスト *)
let () =
  let syuten_eki = (Ex2005.dijkstra "ikebukuro" "iidabashi") in
  print_eki syuten_eki;
  ()