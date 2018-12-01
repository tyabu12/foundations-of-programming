let check_raise f exn =
  try f (); false
  with exn' -> exn' = exn
