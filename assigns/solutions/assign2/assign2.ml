
type int_list_or_string_list
  = IntList of int list
  | StringList of string list

type int_or_string
  = Int of int
  | String of string

let convert (l : int_or_string list) : int_list_or_string_list list =
  let rec go l =
    match l with
    | [] -> []
    | Int n :: xs ->
      (match go xs with
       | IntList ys :: rest -> IntList (n :: ys) :: rest
       | l -> IntList [n] :: l)
    | String s :: xs ->
      (match go xs with
       | StringList ys :: rest -> StringList (s :: ys) :: rest
       | l -> StringList [s] :: l)
  in go l

type recipe = {
  name : string ;
  ingrs : string list;
}

let rec subset (l : 'a list) (r : 'a list) : bool =
  match l with
  | [] -> true
  | x :: xs -> List.mem x r && subset xs r

let recipes_by_ingrs (l : recipe list) (s : string list) : recipe list =
  let rec go l =
    match l with
    | [] -> []
    | r :: rs ->
      if subset r.ingrs s
      then r :: go rs
      else go rs
  in go l

type mem_status =
  | Free
  | Occupied

type memory = (mem_status * int) list

type alloc_result =
  | Success of int * memory
  | Invalid_size


let allocate (size : int) (mem : memory) =
  let cons (stat, i) r =
    match r with
    | Success (p, mem) -> Success (p + i, (stat, i) :: mem)
    | Invalid_size -> Invalid_size
  in
  if size <= 0
  then Invalid_size
  else
    let rec go mem =
      match mem with
      | [] -> Success (0, [Occupied, size])
      | (Occupied, n) :: mem -> cons (Occupied, n) (go mem)
      | (Free, n) :: mem ->
         if size < n
         then Success (0, (Occupied, size) :: (Free, n - size) :: mem)
         else if size = n
         then Success (0, (Occupied, size) :: mem)
         else cons (Free, n) (go mem)
    in go mem

type free_result =
  | Success of memory
  | Invalid_position

let rec free p mem =
  let cons chunk r =
    match r with
    | Invalid_position -> Invalid_position
    | Success mem -> Success (chunk :: mem)
  in
  match mem with
  | [] -> Invalid_position
  | (Free, n) :: mem ->
     if p <= 0
     then Invalid_position
     else (
       match free (p - n) mem with
       | Invalid_position -> Invalid_position
       | Success [] -> Success []
       | Success ((Free, m) :: mem) -> Success ((Free, n + m) :: mem)
       | Success mem -> Success ((Free, n) :: mem)
     )
  | (Occupied, n) :: mem ->
     if p < 0
     then Invalid_position
     else if p = 0
     then
       match mem with
       | [] -> Success []
       | (Free, m) :: mem -> Success ((Free, n + m) :: mem)
       | mem -> Success ((Free, n) :: mem)
     else
       cons (Occupied, n) (free (p - n) mem)
