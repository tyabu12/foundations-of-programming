open Printf

let jikoshokai name =
  "はじめまして。" ^ name ^ "といいます。よろしくお願いします！"

let () =
  print_endline (jikoshokai "田中太郎");
  print_endline (jikoshokai "田中次郎");
  print_endline (jikoshokai "鈴木一郎");
  ()

