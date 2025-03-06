type op = Add | Sub | Mul | Div | Pow

type 'a expr =
  {
    expr : 'a _expr;
    meta : 'a
  }
and 'a _expr =
  | Num of int
  | Op of op * 'a expr * 'a expr

type error_kind =
  | DivByZero
  | NegExp

type 'a error =
  {
    error: error_kind;
    meta : 'a;
  }

let guard b error = if b then Error error else Ok ()

let rec eval (e : 'a expr) : (int, 'a error) result =
  let ( let* ) = Result.bind in
  
  match e.expr with
  | Num n -> Ok n
  | Op (op, e1, e2) ->
      let* v1 = eval e1 in
      let* v2 = eval e2 in
      
      match op with
      | Add -> Ok (v1 + v2)
      | Sub -> Ok (v1 - v2)
      | Mul -> Ok (v1 * v2)
      | Div ->
          let* () = guard (v2 = 0) {error = DivByZero; meta = e.meta} in
          Ok (v1 / v2)
      | Pow ->
          let* () = guard (v2 < 0) {error = NegExp; meta = e.meta} in
          let rec pow_int base exp acc =
            if exp = 0 then acc
            else pow_int base (exp - 1) (acc * base)
          in
          Ok (pow_int v1 v2 1)

exception ListTooShort
exception InvalidArg

let prefix (k : int) (l : 'a list) : 'a list =
  if k < 0 then raise InvalidArg
  else
    let rec take n lst acc =
      match n, lst with
      | 0, _ -> List.rev acc
      | _, [] -> raise ListTooShort
      | n, hd :: tl -> take (n-1) tl (hd :: acc)
    in
    take k l []

type prefix_error =
  | ListTooShort
  | InvalidArg

let prefix_res (k : int) (l : 'a list) : ('a list, prefix_error) result =
  if k < 0 then Error InvalidArg
  else
    let rec take n lst acc =
      match n, lst with
      | 0, _ -> Ok (List.rev acc)
      | _, [] -> Error ListTooShort
      | n, hd :: tl -> take (n-1) tl (hd :: acc)
    in
    take k l []

module type DEQUEUE = sig
  type 'a t
  val empty: 'a t
  val push_front : 'a -> 'a t -> 'a t
  val pop_front : 'a t -> ('a * 'a t) option
  val push_back : 'a -> 'a t -> 'a t
  val pop_back : 'a t -> ('a * 'a t) option
  val to_list : 'a t -> 'a list
end

module ListDequeue = struct
  type 'a t = 'a list
  let empty = []
  let push_front x l = x :: l
  let pop_front l = 
    match l with
    | [] -> None
    | hd :: tl -> Some (hd, tl)
  let push_back x l = l @ [x]
  let pop_back l = 
    match List.rev l with
    | [] -> None
    | hd :: tl -> Some (hd, List.rev tl)
  let to_list l = l
end

module DoubleListDequeue = struct
  type 'a t = 'a list * 'a list
  
  let empty = ([], [])
  
  let balance (front, back) =
    match front, back with
    | [], back -> 
        let back_rev = List.rev back in
        let len = List.length back_rev in
        let front_half = List.filteri (fun i _ -> i < len / 2) back_rev in
        let back_half = List.filteri (fun i _ -> i >= len / 2) back_rev in
        (front_half, List.rev back_half)
    | front, [] -> 
        let len = List.length front in
        let front_half = List.filteri (fun i _ -> i < len / 2) front in
        let back_half = List.filteri (fun i _ -> i >= len / 2) front in
        (front_half, List.rev back_half)
    | front, back -> (front, back)
  
  let push_front x l =
    let (front, back) = l in
    (x :: front, back)
  
  let pop_front l =
    match l with
    | ([], []) -> None
    | ([], back) -> 
        let (front, back) = balance ([], back) in
        (match front with
         | [] -> None
         | hd :: tl -> Some (hd, (tl, back)))
    | (hd :: tl, back) -> Some (hd, (tl, back))
  
  let push_back x l =
    let (front, back) = l in
    (front, x :: back)
  
  let pop_back l =
    match l with
    | ([], []) -> None
    | (front, []) -> 
        let (front, back) = balance (front, []) in
        (match back with
         | [] -> None
         | hd :: tl -> Some (hd, (front, tl)))
    | (front, hd :: tl) -> Some (hd, (front, tl))
  
  let to_list l =
    let (front, back) = l in
    front @ List.rev back
end

module StringMap = Map.Make(String)
module IntMap = Map.Make(Int)
module StringSet = Set.Make(String)

let flip_keys_and_values (m : int StringMap.t) : StringSet.t IntMap.t =
  StringMap.fold
    (fun key value acc ->
      let existing_set = 
        match IntMap.find_opt value acc with
        | Some set -> set
        | None -> StringSet.empty
      in
      let updated_set = StringSet.add key existing_set in
      IntMap.add value updated_set acc)
    m
    IntMap.empty
