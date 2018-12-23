module Heap : sig

  (* 'k : key, 'v : value *)
  type ('k, 'v) t

  type index_t

  (* create size key value *)
  (* ヒープサイズとダミー要素を受け取り, 空のヒープを返す *)
  val create : int -> 'k -> 'v -> ('k, 'v) t

  (* insert heap key value *)
  (* ヒープに要素を追加する (破壊的) *)
  val insert : ('k, 'v) t -> 'k -> 'v -> index_t * ('k, 'v) t

  (* get heap index *)
  (* heap の index 番目の要素を返す *)
  val get : ('k, 'v) t -> index_t -> 'k * 'v

  (* set heap index key value *)
  (* heap の index 番目の値を更新したヒープを返す (破壊的) *)
  val set : ('k, 'v) t -> index_t -> 'k -> 'v -> ('k, 'v) t

  (* split_top heap *)
  (* key が最小の要素と, それを取り除いたヒープを返す (破壊的) *)
  val split_top : ('k, 'v) t -> ('k * 'v) * ('k, 'v) t

  (* map f heap *)
  val map : ('k -> 'v -> 'k * 'v) -> ('k, 'v) t -> ('k, 'v) t

  (* print heap *)
  (* heap を表示する (デバッグ用) *)
  val print : ('k, 'v) t -> ('k -> string) -> ('v -> string) -> unit

  (* length heap *)
  (* heap の要素数を返す *)
  val length : ('k, 'v) t -> int

end = struct

  (* 要素番号 *)
  type index_t = int option ref

  (* 要素が入っているかのフラグ, key, value のタプルの配列, 有効な要素数 *)
  type ('k, 'v) t = (index_t * 'k * 'v) array * int ref

  (* int -> 'k -> 'v -> ('k, 'v) t *)
  let create n k v =
    (Array.make n ((ref None), k, v)), ref 0

  (* ('k, 'v) t -> index_t -> 'k * 'v *)
  let get (a, n) idx = match !idx with
    | None -> invalid_arg "Heap.get"
    | Some ofs ->
      if ofs > !n then invalid_arg "Heap.get"
      else
        let ofs', k, v = a.(ofs) in
        assert (!ofs' = Some ofs);
        (k, v)

  (* 配列の idx1 と idx2 を入れ替えた配列を返す (破壊的) *)
  (* (index_t * 'k * 'v) array -> int -> int -> (index_t * 'k * 'v) array *)
  let swap a ofs1 ofs2 =
    let (idx1, _, _) as e1 = a.(ofs1) in
    let (idx2, _, _) as e2 = a.(ofs2) in
    assert (!idx1 = Some ofs1 && !idx2 = Some ofs2);
    idx1 := Some ofs2; a.(ofs1) <- e2;
    idx2 := Some ofs1; a.(ofs2) <- e1;
    a

  (* 上向きに整理 (破壊的) *)
  (* (index_t * 'k * 'v) array -> index_t -> (index_t * 'k * 'v) array *)
  let rec normarize_up a ofs =
    if ofs = 0 then a
    else
      let p_ofs = (ofs - 1) / 2 in
      let idx, k, _ = a.(ofs) in
      let p_idx, pk, _ = a.(p_ofs) in
      assert (!idx = Some ofs && !p_idx = Some p_ofs);
      if pk <= k then a
      else normarize_up (swap a ofs p_ofs) p_ofs

  (* 下向きに整理 (破壊的) *)
  (* (index_t * 'k * 'v) array -> index_t -> int -> (index_t * 'k * 'v) array *)
  let rec normarize_down a ofs n =
    if ofs >= n then a
    else
      let l_ofs = 2 * (ofs + 1) - 1 in
      if l_ofs >= n then a (* has no childlen *)
      else
        let r_ofs = l_ofs + 1 in
        if r_ofs >= n then
          (* left has childlen, right has no childlen *)
          let idx, k, _ = a.(ofs) in
          let l_idx, lk, _ = a.(l_ofs) in
          assert (!idx = Some ofs && !l_idx = Some l_ofs);
          if k <= lk then a
          else normarize_down (swap a ofs l_ofs) l_ofs n
        else
          (* left and right have childlen *)
          let idx, k, _ = a.(ofs) in
          let l_idx, lk, _ = a.(l_ofs) in
          let r_idx, rk, _ = a.(r_ofs) in
          assert (!idx = Some ofs);
          assert (!l_idx = Some l_ofs && !r_idx = Some r_ofs);
          if k <= lk && k <= rk then
            a
          else
            let swap_ofs = if lk <= rk then l_ofs else r_ofs in
            normarize_down (swap a ofs swap_ofs) swap_ofs n

  (* ('k, 'v) t -> index_t -> 'k -> 'v -> ('k, 'v) t *)
  let set (a, n) idx k v = match !idx with
    | None -> invalid_arg "Heap.set"
    | Some ofs ->
      let ofs', old_k, _ = a.(ofs) in
      assert (!ofs' = Some ofs);
      a.(ofs) <- (idx, k, v);
      if k = old_k then (a, n)
      else if k < old_k then
        (normarize_up a ofs, n)
      else (* k > old_k *)
        (normarize_down a ofs !n, n)

  (* ('k, 'v) t -> 'k -> 'v -> index_t * ('k, 'v) t *)
  let insert (a, n) k v =
    let ofs = !n in
    if ofs >= Array.length a then
      failwith "Heap.insert: no more elements"
    else
      let idx = ref (Some ofs) in
      a.(ofs) <- (idx, k, v);
      n := !n + 1;
      (idx, ((normarize_up a ofs), n))

  (* ('k, 'v) t -> ('k * 'v) * ('k, 'v) t *)
  let split_top (a, n) =
    if !n = 0 then invalid_arg "Heap.split_top"
    else
      let idx, k, v = a.(0) in
      assert (!idx = Some 0);
      let a = if !n >= 2 then swap a 0 (!n - 1) else a in
      idx := None;
      n := !n - 1;
      ((k, v), (normarize_down a 0 !n, n))

  (* ('k -> 'v -> 'k * 'v) -> ('k, 'v) t -> ('k, 'v) t *)
  let map f (a, n) =
    for i = 0 to !n - 1 do
      let idx, k, v = a.(i) in
      let k, v = f k v in
      a.(i) <- (idx, k, v);
      ()
    done;
    (normarize_down a 0 !n, n)

  (* ('k, 'v) t -> ('k -> string) -> ('v -> string) -> unit *)
  let print (a, n) string_of_k string_of_v =
    let rec aux ofs =
      if ofs >= !n then ()
      else
        let idx, k, v = a.(ofs) in
        assert (!idx = Some ofs);
        print_endline (
          "offset:" ^ (string_of_int ofs) ^ ", " ^
          "key:" ^ (string_of_k k) ^ ", " ^
          "value:" ^ (string_of_v v));
        let l_ofs = 2 * (ofs + 1) - 1 in
        let r_ofs = l_ofs + 1 in
        aux l_ofs;
        aux r_ofs
    in aux 0

  (* ('k, 'v) t -> int *)
  let length (_, n) = !n

end

(* Heap テスト *)
let () =
  let open Heap in
  let open CheckRaise in
  let h = create 10 1 "a" in
  assert (length h = 0);
  let idx3, h = insert h 3 "three" in
  let idx4, h = insert h 4 "four" in
  let idx2, h = insert h 2 "two" in
  let idx1, h = insert h 1 "one" in
  assert (get h idx1 = (1, "one") && length h = 4);
  assert (get h idx2 = (2, "two") && length h = 4);
  assert (get h idx3 = (3, "three") && length h = 4);
  assert (get h idx4 = (4, "four") && length h = 4);
  let (k, v), h = split_top h in
  assert (k = 1 && v = "one" && length h = 3);
  let (k, v), h = split_top h in
  assert (k = 2 && v = "two" && length h = 2);
  let (k, v), h = split_top h in
  assert (k = 3 && v = "three" && length h = 1);
  let (k, v), h = split_top h in
  assert (k = 4 && v = "four" && length h = 0);
  assert (check_raise (fun _ -> split_top h)
    (Invalid_argument "Heap.split_top"));
  ()