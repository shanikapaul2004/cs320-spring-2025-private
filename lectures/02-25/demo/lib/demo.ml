
type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree

module TreeExample = struct
  let l = Leaf
  let n l r = Node ((), l, r)
end

let example = TreeExample.(n (n (n l l) l) (n l l))
