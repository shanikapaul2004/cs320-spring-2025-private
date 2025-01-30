
let first_digit n =
  let is_single_digit n =
    0 <= n && n < 10
  in
  let rec aux n =
    if is_single_digit n
    then n
    else aux (n / 10)
  in
  aux (abs n)
