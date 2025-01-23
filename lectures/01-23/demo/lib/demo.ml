
let is_prime n =
  let rec loop div =
    if div < n then
      if n mod div = 0 then
        false
      else
        loop (div + 1)
    else
      true
  in loop 2 (* IS NOT CORRECT *)

