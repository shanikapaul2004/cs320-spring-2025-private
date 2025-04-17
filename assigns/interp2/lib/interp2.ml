include Utils

let parse (s : string) : prog option =
  match Parser.prog Lexer.read (Lexing.from_string s) with
  | e -> Some e
  | exception _ -> None

let rec desugar (p : prog) : expr =
  match p with
  | [] -> Unit
  | stmts -> desugar_stmts stmts

and desugar_stmts (stmts : toplet list) : expr =
  match stmts with
  | [] -> Unit
  | [{is_rec; name; args; ty; binding}] ->
      let desugared_body = desugar_expr binding in
      let fn_expr = desugar_function args desugared_body in
      Let { is_rec; name; ty; binding = fn_expr; body = Var name }
  | {is_rec; name; args; ty; binding} :: rest ->
      let desugared_body = desugar_expr binding in
      let fn_expr = desugar_function args desugared_body in
      Let { is_rec; name; ty; binding = fn_expr; body = desugar_stmts rest }

and desugar_function (args : (string * ty) list) (body : expr) : expr =
  match args with
  | [] -> body
  | (arg_name, ty) :: rest ->
      Fun (arg_name, ty, desugar_function rest body)

and desugar_expr (e : sfexpr) : expr =
  match e with
  | SUnit -> Unit
  | SBool b -> Bool b
  | SNum n -> Num n
  | SVar x -> Var x
  | SBop (op, e1, e2) -> Bop (op, desugar_expr e1, desugar_expr e2)
  | SLet {is_rec; name; args; ty; binding; body} ->
      let desugared_binding = desugar_expr binding in
      let desugared_body = desugar_expr body in
      let fn_expr = desugar_function args desugared_binding in
      Let { is_rec; name; ty; binding = fn_expr; body = desugared_body }
  | SIf (e1, e2, e3) ->
      If (desugar_expr e1, desugar_expr e2, desugar_expr e3)
  | SFun {args; body} ->
      desugar_function args (desugar_expr body)
  | SApp es ->
      (match es with
       | [] -> failwith "Application requires at least one expression"
       | [e] -> desugar_expr e
       | e :: args ->
           List.fold_left
             (fun acc arg -> App (acc, desugar_expr arg))
             (desugar_expr e)
             args)
  | SAssert e -> Assert (desugar_expr e)

  let rec lookup (x : string) (ctx : (string * ty) list) : ty option =
    match ctx with
    | [] -> None
    | (y, t) :: rest -> if x = y then Some t else lookup x rest  

  let rec type_of_expr (ctx : (string * ty) list) (e : expr) : (ty, error) result =
    match e with
    | Unit -> Ok UnitTy
    | Bool _ -> Ok BoolTy
    | Num _ -> Ok IntTy
    | Var x ->
        (match lookup x ctx with
         | Some t -> Ok t
         | None -> Error (UnknownVar x))
  
         | Bop (op, e1, e2) ->
          (match type_of_expr ctx e1 with
           | Error e -> Error e
           | Ok t1 ->
               match type_of_expr ctx e2 with
               | Error e -> Error e
               | Ok t2 ->
                   let int_ops = [Add; Sub; Mul; Div; Mod] in
                   let comp_ops = [Lt; Lte; Gt; Gte; Eq; Neq] in
                   let bool_ops = [And; Or] in
                   if List.mem op int_ops then
                     if t1 = IntTy then
                       if t2 = IntTy then Ok IntTy
                       else Error (OpTyErrR (op, IntTy, t2))
                     else Error (OpTyErrL (op, IntTy, t1))
                   else if List.mem op comp_ops then
                     if t1 = IntTy then
                       if t2 = IntTy then Ok BoolTy
                       else Error (OpTyErrR (op, IntTy, t2))
                     else Error (OpTyErrL (op, IntTy, t1))
                   else if List.mem op bool_ops then
                     if t1 = BoolTy then
                       if t2 = BoolTy then Ok BoolTy
                       else Error (OpTyErrR (op, BoolTy, t2))
                     else Error (OpTyErrL (op, BoolTy, t1))
                   else failwith "Unknown operator")
      
  
    | If (e1, e2, e3) ->
        (match type_of_expr ctx e1 with
         | Error e -> Error e
         | Ok t_cond ->
             (match type_of_expr ctx e2 with
              | Error e -> Error e
              | Ok t_then ->
                  (match type_of_expr ctx e3 with
                   | Error e -> Error e
                   | Ok t_else ->
                       if t_cond <> BoolTy then Error (IfCondTyErr t_cond)
                       else if t_then <> t_else then Error (IfTyErr (t_then, t_else))
                       else Ok t_then)))
  
    | Fun (x, t_arg, body) ->
        let ctx' = (x, t_arg) :: ctx in
        (match type_of_expr ctx' body with
         | Ok t_body -> Ok (FunTy (t_arg, t_body))
         | Error e -> Error e)
  
    | App (e1, e2) ->
        (match type_of_expr ctx e1 with
         | Error e -> Error e
         | Ok t_fun ->
             (match type_of_expr ctx e2 with
              | Error e -> Error e
              | Ok t_arg ->
                  match t_fun with
                  | FunTy (t_expected, t_ret) ->
                      if t_expected = t_arg then Ok t_ret
                      else Error (FunArgTyErr (t_expected, t_arg))
                  | _ -> Error (FunAppTyErr t_fun)))
  
    | Let { is_rec = false; name; ty; binding; body } ->
        (match type_of_expr ctx binding with
         | Error e -> Error e
         | Ok t_binding ->
             if t_binding = ty then
               type_of_expr ((name, ty) :: ctx) body
             else Error (LetTyErr (ty, t_binding)))
  
    | Let { is_rec = true; name; ty; binding; body } ->
        (match binding with
         | Fun (arg, t_arg, e_body) ->
             let t_fn = ty in
             let ctx' = (name, t_fn) :: ctx in
             let ctx_body = (arg, t_arg) :: ctx' in
             (match type_of_expr ctx_body e_body with
              | Error e -> Error e
              | Ok t_body ->
                  (match t_fn with
                   | FunTy (t1, t2) ->
                       if t1 = t_arg && t2 = t_body then
                         type_of_expr ctx' body
                       else Error (LetTyErr (ty, FunTy (t_arg, t_body)))
                   | _ -> Error (LetRecErr name)))
         | _ -> Error (LetRecErr name))
  
    | Assert e ->
        (match type_of_expr ctx e with
         | Error e -> Error e
         | Ok BoolTy -> Ok UnitTy
         | Ok t -> Error (AssertTyErr t))
  
  let type_of (e : expr) : (ty, error) result =
    type_of_expr [] e

  exception AssertFail
  exception DivByZero
  
  let rec eval_expr (env : dyn_env) (e : expr) : value =
    match e with
    | Unit -> VUnit
    | Bool b -> VBool b
    | Num n -> VNum n
    | Var x ->
        (match Env.find_opt x env with
         | Some v -> v
         | None -> failwith ("Unbound variable: " ^ x))
  
         | Bop (And, e1, e2) ->
          (match eval_expr env e1 with
           | VBool false -> VBool false
           | VBool true -> eval_expr env e2
           | _ -> failwith "Invalid AND")
      
      | Bop (Or, e1, e2) ->
          (match eval_expr env e1 with
           | VBool true -> VBool true
           | VBool false -> eval_expr env e2
           | _ -> failwith "Invalid OR")
      
      | Bop (op, e1, e2) ->
          let v1 = eval_expr env e1 in
          let v2 = eval_expr env e2 in
          (match (op, v1, v2) with
           | (Add, VNum n1, VNum n2) -> VNum (n1 + n2)
           | (Sub, VNum n1, VNum n2) -> VNum (n1 - n2)
           | (Mul, VNum n1, VNum n2) -> VNum (n1 * n2)
           | (Div, VNum _, VNum 0) -> raise DivByZero
           | (Div, VNum n1, VNum n2) -> VNum (n1 / n2)
           | (Mod, VNum _, VNum 0) -> raise DivByZero
           | (Mod, VNum n1, VNum n2) -> VNum (n1 mod n2)
           | (Lt, VNum n1, VNum n2) -> VBool (n1 < n2)
           | (Lte, VNum n1, VNum n2) -> VBool (n1 <= n2)
           | (Gt, VNum n1, VNum n2) -> VBool (n1 > n2)
           | (Gte, VNum n1, VNum n2) -> VBool (n1 >= n2)
           | (Eq, VNum n1, VNum n2) -> VBool (n1 = n2)
           | (Neq, VNum n1, VNum n2) -> VBool (n1 <> n2)
           | (Eq, VBool b1, VBool b2) -> VBool (b1 = b2)
           | (Neq, VBool b1, VBool b2) -> VBool (b1 <> b2)
           | _ -> failwith "Invalid operands")
      
  
    | If (e1, e2, e3) ->
        (match eval_expr env e1 with
         | VBool true -> eval_expr env e2
         | VBool false -> eval_expr env e3
         | _ -> failwith "Non-boolean condition")
  
    | Fun (x, _, body) ->
        VClos { arg = x; body; env; name = None }
  
    | App (e1, e2) ->
        let f = eval_expr env e1 in
        let arg_val = eval_expr env e2 in
        (match f with
         | VClos { arg; body; env = clos_env; name = None } ->
             let env' = Env.add arg arg_val clos_env in
             eval_expr env' body
         | VClos { arg; body; env = clos_env; name = Some fname } ->
             let env' = Env.add fname f (Env.add arg arg_val clos_env) in
             eval_expr env' body
         | _ -> failwith "Tried to apply a non-function")
  
    | Let { is_rec = false; name; binding; body; _ } ->
        let bound_val = eval_expr env binding in
        let env' = Env.add name bound_val env in
        eval_expr env' body
  
    | Let { is_rec = true; name; binding; body; _ } ->
        (match binding with
         | Fun (x, _, e_body) ->
             let rec_clos = VClos { arg = x; body = e_body; env; name = Some name } in
             let env' = Env.add name rec_clos env in
             eval_expr env' body
         | _ -> failwith "let rec must bind a function")
  
    | Assert e ->
        (match eval_expr env e with
         | VBool true -> VUnit
         | VBool false -> raise AssertFail
         | _ -> failwith "Non-boolean assert")
  
  let eval (e : expr) : value =
    eval_expr Env.empty e
  
    let interp (s : string) : (value, error) result =
      match parse s with
      | None -> Error ParseErr
      | Some prog ->
          let core_expr = desugar prog in
          match type_of core_expr with
          | Error e -> Error e
          | Ok _ -> Ok (eval core_expr)
    
    
    