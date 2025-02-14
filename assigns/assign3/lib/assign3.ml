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



let fib3_tail ((a, b, c) : int * int * int) (n : int) : int =
  let rec aux n prev3 prev2 prev1 =
    if n = 0 then prev3
    else if n = 1 then prev2
    else if n = 2 then prev1
    else aux (n - 1) prev2 prev1 (prev3 + prev2 + prev1)
  in
  aux n a b c

  let file_tree (root : string) (paths : string list) : string ntree =
    let rec insert_path (Node (name, children)) path_parts =
      match path_parts with
      | [] -> Node (name, children)
      | part :: rest ->
        let rec find_and_update acc = function
          | [] -> List.rev (Node (part, []) :: acc) (* Create new node *)
          | (Node (n, _) as child) :: cs when n = part -> (* `_` replaces unused `sub_children` *)
            List.rev ((insert_path child rest) :: acc) @ cs
          | child :: cs -> find_and_update (child :: acc) cs
        in
        Node (name, find_and_update [] children)
    in
    let split_path (path : string) : string list =
      let rec aux acc i j =
        if j >= String.length path then
          if i < j then List.rev (String.sub path i (j - i) :: acc) else List.rev acc
        else if String.get path j = '/' then
          aux (String.sub path i (j - i) :: acc) (j + 1) (j + 1)
        else aux acc i (j + 1)
      in
      aux [] 0 0
    in
    List.fold_left (fun acc path -> insert_path acc (split_path path)) (Node (root, [])) paths
  
type expr =
  | Num of int
  | Var of string
  | Add of expr * expr
  | Mul of expr * expr

let subst (_e1 : expr) (_x : string) (_e2 : expr) : expr = assert false

let string_of_expr (_e : expr) : string = assert false
