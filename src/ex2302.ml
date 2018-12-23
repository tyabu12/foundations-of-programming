open Ex2301

(* 'a list -> 'a list *)
let heap_sort l = match l with
  | [] -> []
  | hd :: _ ->
    let len = List.length l in
    let h = Heap.create len hd () in
    List.iter (fun e -> let _ = Heap.insert h e () in ()) l;
    assert (Heap.length h = len);
    List.init len (fun _ -> let (k, _), _ = Heap.split_top h in k)

(* テスト *)
let () =
  assert (heap_sort [] = []);
  assert (heap_sort [2] = [2]);
  assert (heap_sort [1; 2] = [1; 2]);
  assert (heap_sort [2; 1] = [1; 2]);
  assert (heap_sort [10; 2; 5; 1; -1; 0] = [-1; 0; 1; 2; 5; 10]);
  ()