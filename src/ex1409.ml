open Ex0803

let () =
  let p1 = {name = "Alice"; height = 172; weight = 66; birth_month = 4; birth_day = 2; blood_type = "B"} in
  assert ((fun p -> p.name) p1 = "Alice");
  assert ((fun p -> p.height) p1 = 172);
  assert ((fun p -> p.weight) p1 = 66);
  assert ((fun p -> p.birth_day) p1 = 2);
  ()
