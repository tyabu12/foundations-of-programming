(* キーが 'a 型, 値が 'b 型の木の型 *)
type ('a, 'b) t

(* empty *)
(* 空の木 *)
val empty : ('a, 'b) t

(* insert tree key value *)
(* 木に key と 値 value を挿入した木を返す *)
val insert : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t

(* search tree key *)
(* 木の中から key に対応する値を探して返す *)
val search : ('a, 'b) t -> 'a -> 'b

(* 木を文字列に変換する (デバッグ用) *)
val to_string : ('a, 'b) t -> ('a -> string) -> ('b -> string) -> string