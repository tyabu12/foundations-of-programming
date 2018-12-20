(* 文字列を受け取り, その文字列に呼ばれるごとに異なる数字をつけた文字列を返す *)
(* gensym : string -> string *)
let cnt = ref (-1)
let gensym s =
  cnt := !cnt + 1;
  s ^ (string_of_int !cnt)

(* テスト *)
let () =
  assert (gensym "a" = "a0");
  assert (gensym "a" = "a1");
  assert (gensym "x" = "x2");
  ()