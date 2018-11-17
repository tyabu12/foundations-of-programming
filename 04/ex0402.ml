let jikoshokai name =
  "はじめまして。" ^ name ^ "といいます。よろしくお願いします！"

let () =
  assert (jikoshokai "田中太郎" =
    "はじめまして。田中太郎といいます。よろしくお願いします！");
  assert (jikoshokai "田中次郎" =
    "はじめまして。田中次郎といいます。よろしくお願いします！");
  assert (jikoshokai "鈴木一郎" =
    "はじめまして。鈴木一郎といいます。よろしくお願いします！");
  ()

