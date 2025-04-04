include Utils

let parse (s : string) : expr option =
  match Parser.prog Lexer.read (Lexing.from_string s) with
  | e -> Some e
  | exception _ -> None

(* 
 * Subst function - replaces occurrences of a variable with a value
 * [v/x]e means "substitute value v for variable x in expression e"
 *)
let rec subst (v : value) (x : string) (e : expr) : expr =
  match e with
  | Unit -> Unit
  | True -> True
  | False -> False
  | Num n -> Num n
  | Var y -> if x = y then
              match v with
              | VNum n -> Num n
              | VBool b -> if b then True else False
              | VUnit -> Unit
              | VFun (param, body) -> Fun (param, body)
            else
              Var y
  | App (e1, e2) -> App (subst v x e1, subst v x e2)
  | Bop (op, e1, e2) -> Bop (op, subst v x e1, subst v x e2)
  | If (e1, e2, e3) -> If (subst v x e1, subst v x e2, subst v x e3)
  | Let (y, e1, e2) -> if x = y then
                         Let (y, subst v x e1, e2)
                       else
                         Let (y, subst v x e1, subst v x e2)
  | Fun (y, e1) -> if x = y then
                     Fun (y, e1)
                   else
                     Fun (y, subst v x e1)

(* Helper function to evaluate binary operations *)
let eval_bop (op : bop) (v1 : value) (v2 : value) : (value, error) result =
  match op, v1, v2 with
  (* Arithmetic operations *)
  | Add, VNum n1, VNum n2 -> Ok (VNum (n1 + n2))
  | Sub, VNum n1, VNum n2 -> Ok (VNum (n1 - n2))
  | Mul, VNum n1, VNum n2 -> Ok (VNum (n1 * n2))
  | Div, VNum n1, VNum n2 -> 
      if n2 = 0 then Error DivByZero
      else Ok (VNum (n1 / n2))
  | Mod, VNum n1, VNum n2 -> 
      if n2 = 0 then Error DivByZero
      else Ok (VNum (n1 mod n2))
    
  (* Comparison operations *)
  | Lt, VNum n1, VNum n2 -> Ok (VBool (n1 < n2))
  | Lte, VNum n1, VNum n2 -> Ok (VBool (n1 <= n2))
  | Gt, VNum n1, VNum n2 -> Ok (VBool (n1 > n2))
  | Gte, VNum n1, VNum n2 -> Ok (VBool (n1 >= n2))
  | Eq, VNum n1, VNum n2 -> Ok (VBool (n1 = n2))
  | Neq, VNum n1, VNum n2 -> Ok (VBool (n1 <> n2))
    
  (* Boolean operations *)
  | And, VBool b1, VBool b2 -> Ok (VBool (b1 && b2))
  | Or, VBool b1, VBool b2 -> Ok (VBool (b1 || b2))
    
  (* Type errors *)
  | (Add | Sub | Mul | Div | Mod | Lt | Lte | Gt | Gte | Eq | Neq), _, _ -> 
      Error (InvalidArgs op)
  | (And | Or), _, _ -> 
      Error (InvalidArgs op)

(* Define the evaluation functions with proper mutual recursion *)
let rec eval (e : expr) : (value, error) result =
  match e with
  | Unit -> Ok VUnit
  | True -> Ok (VBool true)
  | False -> Ok (VBool false)
  | Num n -> Ok (VNum n)
  | Var x -> Error (UnknownVar x)  (* Free variables are errors *)
  
  (* Binary operations *)
  | Bop (And, e1, e2) -> eval_and e1 e2
  | Bop (Or, e1, e2) -> eval_or e1 e2
  | Bop (op, e1, e2) ->
      (* Evaluate operands from left to right *)
      (match eval e1 with
      | Ok v1 ->
          (match eval e2 with
          | Ok v2 -> eval_bop op v1 v2
          | Error e -> Error e)
      | Error e -> Error e)
  
  (* Conditionals *)
  | If (e1, e2, e3) ->
      (match eval e1 with
      | Ok (VBool true) -> eval e2
      | Ok (VBool false) -> eval e3
      | Ok _ -> Error InvalidIfCond
      | Error e -> Error e)
  
  (* Variable binding *)
  | Let (x, e1, e2) ->
      (match eval e1 with
      | Ok v -> 
          let subst_e2 = subst v x e2 in
          eval subst_e2
      | Error e -> Error e)
  
  (* Function definition *)
  | Fun (x, e1) -> Ok (VFun (x, e1))
  
  (* Function application *)
  | App (e1, e2) ->
      (match eval e1 with
      | Ok (VFun (x, body)) ->
          (match eval e2 with
          | Ok v ->
              let subst_body = subst v x body in
              eval subst_body
          | Error e -> Error e)
      | Ok _ -> Error InvalidApp
      | Error e -> Error e)

(* Helper function for evaluating logical AND with short-circuit *)
and eval_and (e1 : expr) (e2 : expr) : (value, error) result =
  match eval e1 with
  | Ok (VBool false) -> Ok (VBool false)
  | Ok (VBool true) -> 
      (match eval e2 with
      | Ok (VBool b) -> Ok (VBool b)
      | Ok _ -> Error (InvalidArgs And)
      | Error e -> Error e)
  | Ok _ -> Error (InvalidArgs And)
  | Error e -> Error e

(* Helper function for evaluating logical OR with short-circuit *)
and eval_or (e1 : expr) (e2 : expr) : (value, error) result =
  match eval e1 with
  | Ok (VBool true) -> Ok (VBool true)
  | Ok (VBool false) -> 
      (match eval e2 with
      | Ok (VBool b) -> Ok (VBool b)
      | Ok _ -> Error (InvalidArgs Or)
      | Error e -> Error e)
  | Ok _ -> Error (InvalidArgs Or)
  | Error e -> Error e

(* Parse and evaluate a string *)
let interp (s : string) : (value, error) result =
  match parse s with
  | Some expr -> eval expr
  | None -> Error ParseFail