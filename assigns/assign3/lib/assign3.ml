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
    (* Helper function: Finds the next '/' index manually *)
    let find_next_slash (path : string) (start : int) : int option =
      let rec aux i =
        if i >= String.length path then None
        else if String.get path i = '/' then Some i
        else aux (i + 1)
      in
      aux start
    in
  
    (* Helper function: Splits a path into parts manually *)
    let split_path (path : string) : string list =
      let rec aux acc i =
        match find_next_slash path i with
        | Some j -> aux (String.sub path i (j - i) :: acc) (j + 1)
        | None -> List.rev (String.sub path i (String.length path - i) :: acc)
      in
      aux [] 0
    in
  
    (* Inserts a path into the tree *)
    let rec insert_path (Node (name, children) as tree) path_parts =
      match path_parts with
      | [] -> tree (* Base case *)
      | part :: rest ->
          let rec update_children updated = function
            | [] -> Node (name, List.rev (Node (part, []) :: updated))
            | (Node (n, _) as child) :: cs when n = part ->
                Node (name, List.rev (insert_path child rest :: updated) @ cs)
            | child :: cs -> update_children (child :: updated) cs
          in
          update_children [] children
    in
  
    (* Build the tree by inserting each path *)
    List.fold_left (fun acc path -> insert_path acc (split_path path)) (Node (root, [])) paths
  
  
  

  
type expr =
  | Num of int
  | Var of string
  | Add of expr * expr
  | Mul of expr * expr

let subst (_e1 : expr) (_x : string) (_e2 : expr) : expr = assert false

let string_of_expr (_e : expr) : string = assert false
