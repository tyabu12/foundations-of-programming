(* 目的: 受け取った lst をクイックソートを使って昇順に整列する *)
let rec quick_sort lst =
  (* 目的: lst の中から n より p である要素のみを取り出す *)
  (* take : int -> int list -> (int -> int -> bool) -> int list *)
  let take n lst p = List.filter (fun item -> p item n) lst
  (* 目的: lst の中から n より小さい要素のみを取り出す *)
  in let take_less n lst = take n lst (<)
  (* 目的: lst の中から n より大きい要素のみを取り出す *)
  in let take_greater n lst = take n lst (>)
  in match lst with
     | [] -> []
     | first :: rest -> quick_sort (take_less first rest)
                        @ [first]
                        @ quick_sort (take_greater first rest)

(* 目的: 受け取った lst をクイックソートを使って昇順に整列する (バグフィックス版) *)
let rec quick_sort2 lst =
  (* 目的: lst の中から n より p である要素のみを取り出す *)
  (* take : int -> int list -> (int -> int -> bool) -> int list *)
  let take n lst p = List.filter (fun item -> p item n) lst
  (* 目的: lst の中から n 以下の要素のみを取り出す *)
  in let take_less_or_eq n lst = take n lst (<=)
  (* 目的: lst の中から n より大きい要素のみを取り出す *)
  in let take_greater n lst = take n lst (>)
  in match lst with
     | [] -> []
     | first :: rest -> quick_sort2 (take_less_or_eq first rest)
                        @ [first]
                        @ quick_sort2 (take_greater first rest)

(* テスト *)
let () =
  assert (quick_sort  [3; 2; 1] = [1; 2; 3]);
  assert (quick_sort2 [3; 2; 1] = [1; 2; 3]);
  assert (quick_sort  [3; 2; 2; 1] = [1; 2; 3]);
  assert (quick_sort2 [3; 2; 2; 1] = [1; 2; 2; 3]);
  ()
