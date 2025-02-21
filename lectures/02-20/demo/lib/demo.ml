
type error = LessThanOne | MoreThanOne

let exactly_one (f : 'a -> bool) (l : 'a list) : ('a, error) result =
  let rec go (found : 'a option) (l : 'a list) : ('a, error) result =
    match l with
    | [] -> (
      match found with
      | None -> Error LessThanOne
      | Some x -> Ok x
    )
    | x :: xs ->
      if f x
      then
        match found with
        | None -> go (Some x) xs
        | Some _ -> Error MoreThanOne
      else go found xs
  in go None l

let rec insert (l : ('a * 'b) list) (key : 'a) (value : 'b) : (('a * 'b) list, 'b) result =
  match l with
  | [] -> Ok [(key, value)]
  | (k, v) :: tail ->
    if key = k
    then Error v
    else Result.map (fun l -> (k, v) :: l) (insert tail key value)

let l =
  let ( let* ) = Result.bind in
  let out = [] in
  let* out = insert out 1 1 in
  let* out = insert out 2 2 in
  let* out = insert out 3 3 in
  Ok out
