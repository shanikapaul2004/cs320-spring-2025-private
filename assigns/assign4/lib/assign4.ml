
let curry = assert false
let uncurry = assert false

let split_list (_l : ('a * 'b) list) : 'a list * 'b list =
  assert false

let split_tree (_t : ('a * 'b) ntree) : 'a ntree * 'b ntree =
  assert false

let filter_map (_f : 'a -> 'b option) (_l : 'a list) : 'b list =
  assert false

let tree_filter (_p : 'a -> 'bool) (_t : 'a ntree) : 'a ntree option =
  assert false

type rat = {
    num : int;
    denom : int;
  }

type distr = (int * rat) list

let random_walk (_walk : int -> int list) (_start : int) (_num_steps: int) : distr =
  assert false
