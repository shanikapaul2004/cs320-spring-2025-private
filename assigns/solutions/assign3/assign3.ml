
type 'a tree =
  | Leaf
  | Node2 of 'a * 'a tree * 'a tree

let ntree_of_tree =
  let rec go t =
    match t with
    | Leaf -> None
    | Node2 (x, left, right) -> Some (Node (x, go' left @ go' right))
  and go' t =
    match go t with
    | None -> []
    | Some t -> [t]
  in go

let fib3_tail (a, b, c) =
  let rec go a b c n =
    if n = 0
    then a
    else go b c (a + b + c) (n - 1)
  in go a b c

let split_on_char ?(ignore_trailing=false) (c : char) (s : string) : string list =
  let len = String.length s in
  let sub curr next = String.sub s curr (next - curr) in
  let rec go curr next =
    if curr >= len
    then if ignore_trailing then [] else [""]
    else if next >= len
    then [sub curr next]
    else if s.[next] = c
    then sub curr next :: go (next + 1) (next + 1)
    else go curr (next + 1)
  in go 0 0

let file_tree root =
  let value (Node (x, _)) = x in
  let rec insert path (Node (root, children)) =
    let rec inserts head path children =
      match children with
      | [] -> [insert path (Node (head, []))]
      | c :: children ->
         if head = value c
         then insert path c :: children
         else c :: inserts head path children
    in
    let children =
      match path with
      | [] -> children
      | head :: path ->
         inserts head path children
    in Node (root, children)
  in
  let rec go acc fs =
    match fs with
    | [] -> acc
    | f :: fs ->
       let path = split_on_char '/' f in
       go (insert path acc) fs
  in go (Node (root, []))

type expr =
  | Num of int
  | Var of string
  | Add of expr * expr
  | Mul of expr * expr

let subst e1 x =
  let rec go e =
    match e with
    | Num n -> Num n
    | Var y ->
       if y = x
       then e1
       else Var y
    | Add (e1, e2) -> Add (go e1, go e2)
    | Mul (e1, e2) -> Mul (go e1, go e2)
  in go

let string_of_expr =
  let rec go drop e =
    let string_of_intp drop n =
      let s = string_of_int n in
      if n >= 0 || drop then s else "(" ^ s ^ ")"
    in
    let go_paren drop e =
      match e with
      | Add (e1, e2) -> "(" ^ go true (Add (e1, e2)) ^ ")"
      | e -> go drop e
    in
    match e with
    | Num n -> string_of_intp drop n
    | Var x -> x
    | Add (e1, e2) -> go drop e1 ^ " + " ^ go false e2
    | Mul (e1, e2) -> go_paren drop e1 ^ " * " ^ go_paren false e2
  in
  go true
