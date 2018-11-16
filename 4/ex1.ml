open Printf

let baito_kyuyo y h =
  (850 + (y * 100))

(* テスト *)
let () =
  assert (baito_kyuyo 0 3 = 850);
  assert (baito_kyuyo 1 3 = 950);
  assert (baito_kyuyo 2 3 = 1050);
  assert (baito_kyuyo 3 3 = 1150);
  ()

