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

let prefix (k : int) (l : 'a list) : 'a list = assert false

type prefix_error =
  | ListTooShort
  | InvalidArg

let prefix_res (k : int) (l : 'a list) : ('a list, prefix_error) result =
  assert false

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
  let empty = assert false
  let push_front x l = assert false
  let pop_front l = assert false
  let push_back x l = assert false
  let pop_back l = assert false
  let to_list l = assert false
end

module DoubleListDequeue = struct
  type 'a t = 'a list * 'a list
  let empty = assert false
  let push_front x l = assert false
  let pop_front l = assert false
  let push_back x l = assert false
  let pop_back l = assert false
  let to_list l = assert false
end

module StringMap = Map.Make(String)
module IntMap = Map.Make(Int)
module StringSet = Set.Make(String)

let flip_keys_and_values (m : int StringMap.t) : StringSet.t IntMap.t =
  assert false
