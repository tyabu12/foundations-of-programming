(* 年号 *)
type nengou_t =
  | Meiji of int  (* 明治 *)
  | Taisho of int (* 大正 *)
  | Showa of int  (* 昭和 *)
  | Heisei of int (* 平成 *)

(* 目的: 年号を受け取り, 対応する西暦年を返す *)
(* to_seireki : nengout_t -> int *)
let to_seireki nengou = match nengou with
  | Meiji (n) -> n + 1867
  | Taisho (n) -> n + 1911
  | Showa (n) -> n + 1925
  | Heisei (n) -> n + 1988

(* 目的: 誕生年 b と現在年 p を受け取り, 年齢を返す *)
(* nenrei : nengou_t -> nengou_t -> int *)
let nenrei b p =
  to_seireki p - to_seireki b

(* テスト *)
let () =
  let p = Heisei 30 in
  assert (nenrei (Meiji 45) p = 106);
  assert (nenrei (Taisho 10) p = 97);
  assert (nenrei (Showa 34) p = 59);
  assert (nenrei (Heisei 6) p = 24);
  ()
