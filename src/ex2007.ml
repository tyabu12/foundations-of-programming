module Set : sig

  (* 要素の型が 'a の集合の型 *)
  type 'a t

  (* 空集合 *)
  val empty : 'a t

  (* 要素1つからなる集合 *)
  val singleton : 'a -> 'a t

  (* 和集合 *)
  val union : 'a t -> 'a t -> 'a t

  (* 共通部分 *)
  val inter : 'a t -> 'a t -> 'a t

  (* 差集合 *)
  val diff : 'a t -> 'a t -> 'a t

  (* 要素が集合に属しているか *)
  val mem : 'a -> 'a t -> bool

end = struct

  type 'a t = 'a list

  let empty = []

  let singleton elm = [elm]

  let mem elm set = List.mem elm set

  let union set1 set2 =
    List.fold_left
      (fun set elm -> if mem elm set then set else elm :: set)
      set1 set2

  let inter set1 set2 =
    List.filter (fun elm -> mem elm set2) set1

  let diff set1 set2 =
    List.filter (fun elm -> not (mem elm set2)) set1

end

(* テスト *)
let () =
  let s1 = Set.singleton 1 in
  let s2 = Set.singleton 2 in
  assert (Set.mem 1 s1);
  assert (not (Set.mem 2 s1));
  let s12 = Set.union s1 s2 in
  assert (Set.mem 2 s12);
  assert (not (Set.mem 4 s12));
  assert (Set.mem 1 (Set.inter s12 s1));
  assert (not (Set.mem 3 (Set.inter s12 s1)));
  assert (Set.mem 1 (Set.diff s1 s2));
  assert (not (Set.mem 1 (Set.diff s1 s1)));
  ()