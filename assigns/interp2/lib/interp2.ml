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


  let type_of (_ : expr) : (ty, error) result = assert false

  exception AssertFail
  exception DivByZero
  
  let eval (_ : expr) :  value = assert false
  
  let interp (_ : string) : (value, error) result = assert false