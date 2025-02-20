open Stdlib320 

let curry (f : ('a * 'b) -> 'c) : 'a -> 'b -> 'c =
  fun x y -> f (x, y)

let uncurry (f : 'a -> 'b -> 'c) : ('a * 'b) -> 'c =
  fun (x, y) -> f x y

let split_list (l : ('a * 'b) list) : 'a list * 'b list =
  List.fold_right (fun (x, y) (xs, ys) -> (x :: xs, y :: ys)) l ([], [])

let split_tree (t : ('a * 'b) ntree) : 'a ntree * 'b ntree =
  let rec helper t =
    match t with
    | Node ((x, y), children) ->
        let left_children, right_children =
          List.fold_right
            (fun (l_child, r_child) (l_acc, r_acc) ->
              (l_child :: l_acc, r_child :: r_acc))
            (List.map helper children)
            ([], [])
        in
        (Node (x, left_children), Node (y, right_children))
  in
  helper t

let filter_map (f : 'a -> 'b option) (l : 'a list) : 'b list =
  List.fold_right (fun x acc -> match f x with Some y -> y :: acc | None -> acc) l []

let tree_filter (_p : 'a -> 'bool) (_t : 'a ntree) : 'a ntree option =
  assert false

type rat = {
    num : int;
    denom : int;
  }

type distr = (int * rat) list

let random_walk (_walk : int -> int list) (_start : int) (_num_steps: int) : distr =
  assert false
