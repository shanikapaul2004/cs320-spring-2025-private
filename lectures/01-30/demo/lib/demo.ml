
let rec generate n =
  if n <= 0
  then []
  else n :: generate (n - 1)

let rec length l =
  match l with
  | [] -> 0
  | _ :: xs -> 1 + length xs

(*
len (1 :: 2 :: 3 :: 4 :: 5 :: [])
     |    |||||||||||||||||||||||
     x    xs

(1::2::3::4::5::[]) ====> 1 :: (2::3::4::5::[])

[2::3::4::5::[] / xs][1 / x] (1 + length xs) =
[2::3::4::5::[] / xs] (1 + length xs) =

1 + length (2::3::4::5::[])


match (1::2::3::4::5::[]) with | [] -> 0 | x::xs -> 1 + length xs

1 + len (2 :: 3 :: 4 :: 5 :: [])
1 + 1 + len (3::4::5::[])
...
1 + 1 + 1 + 1 + 1 + len []
1 + 1 + 1 + 1 + 1 + 0
5
*)

let rec double l =
  match l with
  | [] -> []
  | x :: l -> x :: x :: double l

let delete_every_other =
  let rec go l =
    match l with
    | [] -> []
    | [x] -> [x]
    | head1 :: _ :: tail -> head1 :: go tail
  in go
