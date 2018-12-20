type color_t = Red | Black

type ('a, 'b) t =
  | Empty
  | Node of ('a, 'b) t * 'a * 'b * color_t * ('a, 'b) t

(* 空の木 *)
let empty = Empty

(* 目的: 赤黒木 tree を受け取り, 必要なら木を再構成する *)
(* balance : ('a, 'b) t -> ('a, 'b) t *)
let rec balance tree = match tree with
  | Node (
      Node (
        Node (a, xk, xv, Red, b),
        yk, yv, Red,
        c),
      zk, zv, Black,
      d)
  | Node (
      Node (
        a,
        xk, xv, Red,
        Node (b, yk, yv, Red, c)),
      zk, zv, Black,
      d)
  | Node (
      a,
      xk, xv, Black,
      Node (
        Node (b, yk, yv, Red, c),
        zk, zv, Red,
        d))
  | Node (
      a,
      xk, xv, Black,
      Node (
        b,
        yk, yv, Red,
        Node (c, zk, zv, Red, d)))
    -> Node (
        Node (a, xk, xv, Black, b),
        yk, yv, Red,
        Node (c, zk, zv, Black, d))
  | _ -> tree

(* 目的: 赤黒木 tree, key, value を受け取り,それを挿入した赤黒木を返す.
        挿入する key がすでに木に存在する場合は値を更新する *)
(* insert : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t *)
let insert tree k v =
  let rec aux tr = match tr with
    | Empty -> Node (Empty, k, v, Red, Empty)
    | Node (left, key, value, color, right) ->
      if key = k then Node (left, key, v, color, right)
      else
        let tr' =
          if k < key then Node (aux left, key, value, color, right)
          else Node (left, key, value, color, aux right)
        in
        match color with
        | Red -> balance tr'
        | Black -> balance tr'
  in
  match aux tree with
  | Empty -> assert false (* この場合は起こりえない *)
  | Node (left, key, value, _, right) -> Node (left, key, value, Black, right)

(* 目的: 赤黒木 tree と key を受け取り, key に一致する value を返す *)
(* search : ('a 'b) t -> 'b *)
let rec search tree key = match tree with
  | Empty -> raise Not_found
  | Node (left, k, value, _, right) ->
    if k = key then value
    else if key < k then search left key
    else search right key

(* 目的: 木を文字列に変換する (デバッグ用) *)
(* to_string : t -> ('a -> string) -> ('b -> string) -> string *)
let rec to_string tree string_of_key string_of_value =
  let rec aux tree = match tree with
  | Empty -> "()"
  | Node (left, key, value, color, right) ->
    "(" ^
      aux left ^ "," ^
      string_of_key key ^ "," ^
      string_of_value value ^ "," ^
      (if color = Black then "BLK" else "RED") ^ "," ^
      aux right ^
    ")"
  in aux tree