
let curry f a b = f (a, b)
let uncurry f (a, b) = f a b

let rec split_list l =
  match l with
  | [] -> ([], [])
  | (x, y) :: xs ->
     let (l, r) = split_list xs in
     (x :: l, y :: r)

let rec split_tree t =
  match t with
  | Node ((u, v), cs) ->
     let (ls, rs) = split_list (List.map split_tree cs) in
     (Node (u, ls), Node (v, rs))

let filter_map f =
  let rec go l =
    match l with
    | [] -> []
    | x :: l -> (
      match f x with
      | None -> go l
      | Some v -> v :: go l
    )
  in go

let tree_filter p =
  let rec go (Node (v, cs)) =
    if p v
    then Some (Node (v, filter_map go cs))
    else (
      match cs with
      | [] -> None
      | Node (v', cs') :: cs -> go (Node (v', cs @ cs'))
    )
  in go

type rat = {
    num : int;
    denom : int;
  }

let simplify q =
  let rec gcd a b =
    if a = 0
    then b
    else gcd (b mod a) a
  in
  let gcd = gcd q.num q.denom in
  {num = q.num / gcd; denom = q.denom / gcd}

let rat_add q1 q2 = simplify {
    num = q1.num * q2.denom + q2.num * q1.denom;
    denom = q1.denom * q2.denom
  }

type distr = (int * rat) list

let rec insert (d : distr) ((x, q) : int * rat) : distr =
  match d with
  | [] -> [(x, simplify q)]
  | (y, r) :: tail ->
     if x = y
     then (x, rat_add q r) :: tail
     else if x < y
     then (x, simplify q) :: d
     else (y, r) :: insert tail (x, q)

let random_walk (walk : 'a -> 'a list) (start : int) (num_steps: int) : distr =
  let q n d = {num=n;denom=d} in
  let step (i, {num;denom}) =
    let poss = walk i in
    let num_poss = List.length poss in
    List.map (fun x -> (x, q num (denom * num_poss))) poss
  in
  let rec go n =
    if n = 0
    then [(start, q 1 1)]
    else List.concat (List.map step (go (n - 1)))
  in List.fold_left insert [] (go num_steps)
