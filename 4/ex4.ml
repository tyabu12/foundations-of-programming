let bmi h w =
  w /. (h ** 2.)

let () =
  assert (int_of_float ((bmi 1.58 55.) *. 100.) = 2203);
  assert (int_of_float ((bmi 1.68 55.) *. 100.) = 1948);
  assert (int_of_float ((bmi 1.78 55.) *. 100.) = 1735);
  ()

