open Ex1201 (* eki_t *)
open Ex2101 (* print_eki *)

(* List dijkstra *)
(* open Ex1605 *)

(* Binary Tree dijkstra *)
(* open Ex1715 *)

(* RedBlack dijkstra *)
open Ex2005

let main shiten_romaji syuten_romaji =
  let syuten_eki = dijkstra shiten_romaji syuten_romaji in
  print_eki syuten_eki;
  ()

let () =
  if Array.length Sys.argv < 3 then (
    print_endline (Sys.argv.(0) ^ " <shiten_romaji> <syuten_romaji>");
    (* exit 1 *)
  ) else (
    main Sys.argv.(1) Sys.argv.(2);
    exit 0
  )