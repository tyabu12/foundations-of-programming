open Printf

let hyojun_taiju h =
  h ** 2. *. 22.

(* テスト *)
let () =
  assert (int_of_float ((hyojun_taiju 1.58) *. 100.) = 5492);
  assert (int_of_float ((hyojun_taiju 1.68) *. 100.) = 6209);
  assert (int_of_float ((hyojun_taiju 1.78) *. 100.) = 6970);
  ()

