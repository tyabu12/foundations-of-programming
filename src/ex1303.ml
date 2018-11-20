(* f1 : 'a -> 'a *)
let f1 a = a

(* f2 : 'a -> 'b -> 'a *)
let f2 a b = a

(* f3 : 'a -> 'b -> 'b *)
let f3 a b = b

(* f4 : 'a -> ('a -> 'b) -> 'b *)
let f4 a f = f a

(* f5 : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c *)
let f5 f g a = g (f a)
