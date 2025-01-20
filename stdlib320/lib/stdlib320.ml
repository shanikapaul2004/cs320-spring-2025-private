include Stdlib

let print = print_endline
let read () = In_channel.(input_all stdin)

module Int = struct
  include Int
  let sprint = string_of_int
  let print n = n |> sprint |> print
end

module Float = struct
  include Float
  let sprint = string_of_float
  let print n = n |> sprint |> print
end

module Char = struct
  include Char
  let sprint = String.make 1
  let print c = c |> sprint |> print
end

module Bool = struct
  include Bool
  let sprint b = if b then "true" else "false"
  let print b = b |> sprint |> print
end

module List = struct
  include List

  let sprint sprinter l =
    let rec loop = function
      | [] -> ""
      | [x] -> sprinter x
      | x :: l -> sprinter x ^ "; " ^ loop l
    in "[" ^ loop l ^ "]"

  let print sprinter l =
    l |> sprint sprinter |> print
end

module Option = struct
  include Option
  let sprint sprinter = function
    | Some x -> "Some " ^ sprinter x
    | None -> "None"

  let print sprinter o = o |> sprint sprinter |> print
end

type 'a ntree =
  | Node of 'a * 'a ntree list

module Ntree = struct
  let sprint ?(unicode=true) sprinter t =
    let vline = if unicode then "│  " else "|  " in
    let sline = if unicode then "├──" else "|--" in
    let bline = if unicode then "└──" else "\\--" in
    let blank = "   " in
    let rec loop (Node (a, ts)) =
      sprinter a :: loop' [] ts
    and loop' prefix ts =
      let prefix_str =
        List.fold_left
          (fun acc b -> (if b then vline else blank) ^ acc)
          ""
          prefix
      in
      match ts with
      | [] -> []
      | [Node (a, ts)] ->
         [prefix_str ^ bline ^ sprinter a]
         @ loop' (false :: prefix) ts
      | Node (a, cs) :: ts ->
         [prefix_str ^ sline ^ sprinter a]
         @ loop' (true :: prefix) cs
         @ loop' prefix ts
    in
    String.concat "\n" (loop t)

  let print ?(unicode=true) printer t =
    t |> sprint ~unicode printer |> print
end
