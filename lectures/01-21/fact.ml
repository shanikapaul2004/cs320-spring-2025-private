let rec fact (n : int) : int =
  if n = 0
  then 1
  else n * fact (n - 1)

let _ = assert (fact 5 = 120)

let fact n =
  let acc = 1 in
  let i = 1 in
  let rec loop acc i =
    let acc = acc * i in
    let i = i + 1 in
    if i <= n
    then loop acc i (* no return statement *)
    else acc
  in loop acc i

let _ = assert (fact 5 = 120)

let fact n =
  let rec loop acc i =
    if i <= n
    then loop (acc * i) (i + 1)
    else acc
  in loop 1 1

(*

fact 5
loop 1 1
if 1 <= 5 then loop (1 * 1) (1 + 1) else 1
loop 1 2
loop 2 3
loop 6 4
loop 24 5
loop 120 6
if 6 <= 5 then loop (120 * 6) (6 + 1) else 120
120

*)

