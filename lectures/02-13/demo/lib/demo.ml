
let smallest_prime_factor (n : int) : int =
  let rec loop (i : int) : int =
    if n mod i = 0
    then i
    else loop (i + 1)
  in loop 2

let is_prime n = smallest_prime_factor n = n

let products (l : int list) : int list =
  let p (n : int) : bool =
    let x = smallest_prime_factor n in
    let y = n / x in
    x <> y && y <> 1 && is_prime y
  in
  List.filter p l


let append l r =
  List.fold_right
    (fun x acc -> x :: acc) (* op *)
    l
    r (* base *)
(*
  match l with
  | [] -> r
  | x :: xs -> x :: append xs r
*)

let fold_right op l base =
  let rec go l acc =
    match l with
    | [] -> acc
    | x :: xs -> go (op acc x) xs
  in go l base

