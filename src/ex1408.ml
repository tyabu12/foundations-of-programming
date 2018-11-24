let () =
  assert ((fun n -> n * n - 1) 0 = -1);
  assert ((fun n -> n * n - 1) 1 = 0);
  assert ((fun n -> n * n - 1) 2 = 3);
  assert ((fun n -> n * n - 1) (-1) = 0);
  ()
