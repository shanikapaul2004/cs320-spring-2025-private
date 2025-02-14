open Stdlib320 

type 'a tree =
  | Leaf
  | Node2 of 'a * 'a tree * 'a tree

let rec ntree_of_tree = function
  | Leaf -> None
  | Node2 (v, left, right) ->
      let children =
        let cl = ntree_of_tree left in
        let cr = ntree_of_tree right in
        match (cl, cr) with
        | (None, None) -> []
        | (Some l, None) -> [l]
        | (None, Some r) -> [r]
        | (Some l, Some r) -> [l; r]
      in
      Some (Node (v, children))



let fib3_tail (_inits : int * int * int) (_n : int) : int = assert false

let file_tree (_root : string) (_paths : string list) : string ntree = assert false

type expr =
  | Num of int
  | Var of string
  | Add of expr * expr
  | Mul of expr * expr

let subst (_e1 : expr) (_x : string) (_e2 : expr) : expr = assert false

let string_of_expr (_e : expr) : string = assert false
