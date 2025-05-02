include Utils

let parse (s : string) : prog option =
  match Parser.prog Lexer.read (Lexing.from_string s) with
  | prog -> Some prog
  | exception _ -> None

(* Apply a substitution to a type *)
let apply_subst (subst : (string * ty) list) (t : ty) : ty =
  let rec deep_apply t =
    match t with
    | TUnit | TInt | TFloat | TBool -> t
    | TVar x -> 
        (match List.assoc_opt x subst with
         | Some ty -> 
             (* Recursively apply substitutions to handle chains *)
             let ty' = deep_apply ty in
             if ty' = t then t else deep_apply ty'
         | None -> TVar x)
    | TFun (t1, t2) -> TFun (deep_apply t1, deep_apply t2)
    | TPair (t1, t2) -> TPair (deep_apply t1, deep_apply t2)
    | TList t -> TList (deep_apply t)
    | TOption t -> TOption (deep_apply t)
  in deep_apply t

(* Check if a type variable appears in a type *)
let rec occurs (x : string) (t : ty) : bool =
  match t with
  | TUnit | TInt | TFloat | TBool -> false
  | TVar y -> x = y
  | TFun (t1, t2) -> occurs x t1 || occurs x t2
  | TPair (t1, t2) -> occurs x t1 || occurs x t2
  | TList t -> occurs x t
  | TOption t -> occurs x t

(* Compose two substitutions, ensuring full variable resolution *)
let compose_subst (s1 : (string * ty) list) (s2 : (string * ty) list) : (string * ty) list =
  (* Apply s2 to the range of s1 *)
  let s1' = List.map (fun (x, t) -> (x, apply_subst s2 t)) s1 in
  
  (* Add bindings from s2 that aren't already in s1 *)
  let s2' = List.filter (fun (x, _) -> not (List.exists (fun (y, _) -> x = y) s1')) s2 in
  
  (* Combine both substitutions *)
  s1' @ s2'

(* Unify a single constraint, returning a substitution *)
let rec unify_one (t1 : ty) (t2 : ty) : (string * ty) list option =
  match t1, t2 with
  | TUnit, TUnit | TInt, TInt | TFloat, TFloat | TBool, TBool -> Some []
  
  | TVar x, TVar y when x = y -> Some []
  
  | TVar x, t ->
      if occurs x t then None  (* Occur check failure *)
      else Some [(x, t)]
      
  | t, TVar x ->
      if occurs x t then None  (* Occur check failure *)
      else Some [(x, t)]
      
  | TFun (s1, s2), TFun (t1, t2) ->
      (match unify_one s1 t1 with
       | None -> None
       | Some subst1 ->
           let s2' = apply_subst subst1 s2 in
           let t2' = apply_subst subst1 t2 in
           match unify_one s2' t2' with
           | None -> None
           | Some subst2 -> 
               Some (compose_subst subst1 subst2))
               
  | TPair (s1, s2), TPair (t1, t2) ->
      (match unify_one s1 t1 with
       | None -> None
       | Some subst1 ->
           let s2' = apply_subst subst1 s2 in
           let t2' = apply_subst subst1 t2 in
           match unify_one s2' t2' with
           | None -> None
           | Some subst2 -> 
               Some (compose_subst subst1 subst2))
               
  | TList s, TList t ->
      unify_one s t
      
  | TOption s, TOption t ->
      unify_one s t
      
  | _, _ -> None  (* Type mismatch *)

(* Unify a list of constraints, returning a substitution *)
let rec unify (cs : constr list) : (string * ty) list option =
  match cs with
  | [] -> Some []
  | (t1, t2) :: rest ->
      match unify_one t1 t2 with
      | None -> None
      | Some subst1 ->
          (* Apply subst1 to the remaining constraints *)
          let rest' = List.map (fun (t1, t2) -> 
                              (apply_subst subst1 t1, apply_subst subst1 t2)) rest in
          match unify rest' with
          | None -> None
          | Some subst2 -> 
              (* Compose the substitutions to ensure full resolution *)
              Some (compose_subst subst1 subst2)

(* Get free type variables in a type *)
let rec free_vars_ty (t : ty) : VarSet.t =
  match t with
  | TUnit | TInt | TFloat | TBool -> VarSet.empty
  | TVar x -> VarSet.singleton x
  | TFun (t1, t2) -> VarSet.union (free_vars_ty t1) (free_vars_ty t2)
  | TPair (t1, t2) -> VarSet.union (free_vars_ty t1) (free_vars_ty t2)
  | TList t -> free_vars_ty t
  | TOption t -> free_vars_ty t

let principle_type (ty : ty) (cs : constr list) : ty_scheme option =
  match unify cs with
  | None -> None  (* No unifier exists *)
  | Some subst ->
      (* Apply substitution to the type *)
      let t' = apply_subst subst ty in
      
      (* Get the free type variables and create a scheme *)
      let free_vars = free_vars_ty t' in
      let var_list = VarSet.elements free_vars in
      
      if List.length var_list = 0 then
        Some (Forall (VarSet.empty, t'))
      else
        Some (Forall (free_vars, t'))

(*interp3 after the check in*)

(* Substitute a type for a type variable in a given type expression *)
let rec substitute_ty target_ty var_name ty =
  match ty with
  | TUnit | TInt | TFloat | TBool -> ty
  | TVar v -> if v = var_name then target_ty else ty
  | TFun (lhs, rhs) -> TFun (substitute_ty target_ty var_name lhs, substitute_ty target_ty var_name rhs)
  | TPair (lhs, rhs) -> TPair (substitute_ty target_ty var_name lhs, substitute_ty target_ty var_name rhs)
  | TList inner -> TList (substitute_ty target_ty var_name inner)
  | TOption inner -> TOption (substitute_ty target_ty var_name inner)

(* Instantiate a polymorphic type scheme with fresh type variables *)
let instantiate (Forall (vars, base_ty)) =
  let fresh_subst = VarSet.fold (fun v acc -> (v, TVar (gensym ())) :: acc) vars [] in
  List.fold_left (fun t (v, ty) -> apply_subst [(v, ty)] t) base_ty fresh_subst

(* Generalize a type with respect to the free type variables in the environment *)
let generalize (env : stc_env) (ty : ty) : ty_scheme =
  let env_fv = Env.fold (fun _ (Forall (_, t)) acc -> VarSet.union acc (free_vars_ty t)) env VarSet.empty in
  let free_vars = VarSet.diff (free_vars_ty ty) env_fv in
  Forall (free_vars, ty)

(* Main inference function *)
let rec infer_expr (env : stc_env) (e : expr) : (ty * constr list) option =
  let fresh_var () = TVar (gensym ()) in
  match e with
  | Unit -> Some (TUnit, [])
  | Bool _ -> Some (TBool, [])
  | Int _ -> Some (TInt, [])
  | Float _ -> Some (TFloat, [])
  | Nil -> Some (TList (fresh_var ()), [])
  | ENone -> Some (TOption (fresh_var ()), [])
  | Var x ->
      (match Env.find_opt x env with
       | Some scheme -> Some (instantiate scheme, [])
       | None -> None)
  | ESome v ->
      (match infer_expr env v with
       | Some (t, c) -> Some (TOption t, c)
       | None -> None)
  | Bop (op, e1, e2) ->
      (match infer_expr env e1, infer_expr env e2 with
       | Some (t1, c1), Some (t2, c2) ->
           let constraints = match op with
             | Add | Sub | Mul | Div | Mod -> [(t1, TInt); (t2, TInt)]
             | AddF | SubF | MulF | DivF | PowF -> [(t1, TFloat); (t2, TFloat)]
             | Eq | Neq | Lt | Lte | Gt | Gte -> [(t1, t2)]
             | And | Or -> [(t1, TBool); (t2, TBool)]
             | Cons ->
                 let a = fresh_var () in
                 [(t1, a); (t2, TList a)]
             | Comma -> []
           in
           let result_ty = match op with
             | Add | Sub | Mul | Div | Mod -> TInt
             | AddF | SubF | MulF | DivF | PowF -> TFloat
             | Eq | Neq | Lt | Lte | Gt | Gte | And | Or -> TBool
             | Cons ->
                 let a = match constraints with (a, _) :: _ -> a | _ -> fresh_var () in
                 TList a
             | Comma -> TPair (t1, t2)
           in
           Some (result_ty, c1 @ c2 @ constraints)
       | _ -> None)
  | If (cond, thn, els) ->
      (match infer_expr env cond, infer_expr env thn, infer_expr env els with
       | Some (tc, cc), Some (tt, ct), Some (te, ce) ->
           Some (te, (tc, TBool) :: (tt, te) :: (cc @ ct @ ce))
       | _ -> None)
  | Assert inner ->
      if inner = Bool false then Some (fresh_var (), [])
      else
        (match infer_expr env inner with
         | Some (t, c) -> Some (TUnit, (t, TBool) :: c)
         | None -> None)
  | Annot (exp, annotated_ty) ->
      (match infer_expr env exp with
       | Some (t, c) -> Some (annotated_ty, (t, annotated_ty) :: c)
       | None -> None)
  | Fun (x, ty_opt, body) ->
      let param_ty = match ty_opt with
        | Some t -> t
        | None -> fresh_var ()
      in
      let env' = Env.add x (Forall (VarSet.empty, param_ty)) env in
      (match infer_expr env' body with
       | Some (t_body, c_body) -> Some (TFun (param_ty, t_body), c_body)
       | None -> None)
  | App (f, arg) ->
      (match infer_expr env f, infer_expr env arg with
       | Some (tf, cf), Some (ta, ca) ->
           let res_ty = fresh_var () in
           Some (res_ty, (tf, TFun (ta, res_ty)) :: (cf @ ca))
       | _ -> None)
  | Let { is_rec = false; name; binding; body } ->
      (match infer_expr env binding with
       | Some (t_b, c_b) ->
           (match infer_expr (Env.add name (Forall (VarSet.empty, t_b)) env) body with
            | Some (t_body, c_body) -> Some (t_body, c_b @ c_body)
            | None -> None)
       | None -> None)
  | Let { is_rec = true; name; binding = Fun _ as func; body } ->
      let alpha = fresh_var () in
      let env' = Env.add name (Forall (VarSet.empty, alpha)) env in
      (match infer_expr env' func with
       | Some (t_func, c_func) ->
           (match infer_expr env' body with
            | Some (t_body, c_body) -> Some (t_body, (alpha, t_func) :: c_func @ c_body)
            | None -> None)
       | None -> None)
  | Let { is_rec = true; _ } -> None
  | ListMatch { matched; hd_name; tl_name; cons_case; nil_case } ->
      (match infer_expr env matched with
       | Some (t_list, c_match) ->
           let elem_ty = fresh_var () in
           let env' =
             env
             |> Env.add hd_name (Forall (VarSet.empty, elem_ty))
             |> Env.add tl_name (Forall (VarSet.empty, TList elem_ty))
           in
           (match infer_expr env' cons_case, infer_expr env nil_case with
            | Some (tc, cc), Some (tn, cn) ->
                let result_ty = fresh_var () in
                let constraints = [
                  (t_list, TList elem_ty);
                  (tc, result_ty);
                  (tn, result_ty)
                ] in
                Some (result_ty, c_match @ cc @ cn @ constraints)
            | _ -> None)
       | None -> None)
  | OptMatch { matched; some_name; some_case; none_case } ->
      (match infer_expr env matched with
       | Some (t_opt, c_match) ->
           let opt_ty = fresh_var () in
           let env' = Env.add some_name (Forall (VarSet.empty, opt_ty)) env in
           (match infer_expr env' some_case, infer_expr env none_case with
            | Some (ts, cs), Some (tn, cn) ->
                Some (tn, (t_opt, TOption opt_ty) :: (ts, tn) :: c_match @ cs @ cn)
            | _ -> None)
       | None -> None)
  | PairMatch { matched; fst_name; snd_name; case } ->
      (match infer_expr env matched with
       | Some (tpair, cpair) ->
           let l = fresh_var () in
           let r = fresh_var () in
           let env' =
             env
             |> Env.add fst_name (Forall (VarSet.empty, l))
             |> Env.add snd_name (Forall (VarSet.empty, r))
           in
           (match infer_expr env' case with
            | Some (t_case, c_case) ->
                Some (t_case, (tpair, TPair (l, r)) :: cpair @ c_case)
            | None -> None)
       | None -> None)

(* Final type_of function *)
let type_of (ctx : stc_env) (e : expr) : ty_scheme option =
  match infer_expr ctx e with
  | Some (ty, constraints) ->
      (match unify constraints with
       | Some subst ->
           let final_ty = List.fold_left (fun t (x, t') -> substitute_ty t' x t) ty subst in
           Some (generalize ctx final_ty)
       | None -> None)
  | None -> None

(*type of function end *)

let is_well_typed (p : prog) : bool =
  let rec check env = function
    | [] -> true
    | { is_rec; name; binding } :: rest ->
        if is_rec then
          match binding with
          | Fun (_, _, _) ->
              (* Assign a fresh type var and extend env temporarily *)
              let alpha = TVar (gensym ()) in
              let temp_env = Env.add name (Forall (VarSet.empty, alpha)) env in
              begin match type_of temp_env binding with
              | Some (Forall (_, ty)) ->
                  let scheme = generalize env ty in
                  let new_env = Env.add name scheme env in
                  check new_env rest
              | None -> false
              end
          | _ -> false (* Cannot recursively define non-functions *)
        else
          match type_of env binding with
          | Some (Forall (_, ty)) ->
              let scheme = generalize env ty in
              let new_env = Env.add name scheme env in
              check new_env rest
          | None -> false
  in
  check Env.empty p


exception AssertFail
exception DivByZero
exception CompareFunVals

let eval_binop op v1 v2 =
  match op,v1,v2 with
  | Add,   VInt a,   VInt b   -> VInt(a+b)
  | Sub,   VInt a,   VInt b   -> VInt(a-b)
  | Mul,   VInt a,   VInt b   -> VInt(a*b)
  | Div,   VInt a,   VInt b   -> if b=0 then raise DivByZero else VInt(a/b)
  | Mod,   VInt a,   VInt b   -> if b=0 then raise DivByZero else VInt(a mod b)
  | AddF,  VFloat a, VFloat b -> VFloat(a+.b)
  | SubF,  VFloat a, VFloat b -> VFloat(a-.b)
  | MulF,  VFloat a, VFloat b -> VFloat(a*.b)
  | DivF,  VFloat a, VFloat b -> VFloat(a/.b)
  | PowF,  VFloat a, VFloat b -> VFloat(a**b)
  | Lt,    VInt a,   VInt b   -> VBool(a<b)
  | Lte,   VInt a,   VInt b   -> VBool(a<=b)
  | Gt,    VInt a,   VInt b   -> VBool(a>b)
  | Gte,   VInt a,   VInt b   -> VBool(a>=b)
  | Eq,    x,        y        -> VBool(x=y)
  | Neq,   x,        y        -> VBool(x<>y)
  | And,   VBool a,  VBool b  -> VBool(a&&b)
  | Or,    VBool a,  VBool b  -> VBool(a||b)
  | Comma, x,        y        -> VPair(x,y)
  | Cons,  x,        VList l  -> VList(x::l)
  | _ -> failwith "Invalid operands"
  
  let rec eval_expr (env : dyn_env) (e : expr) : value =
    match e with
    | Unit -> VUnit
    | Bool b -> VBool b
    | Int n -> VInt n
    | Float f -> VFloat f
    | Var x -> 
        (match Env.find_opt x env with
        | Some v -> v
        | None -> failwith ("Unbound variable: " ^ x))
    | Fun (x, _, body) -> 
        VClos { name = None; arg = x; body = body; env = env }
    | App (f, a) ->
        let vf = eval_expr env f in
        let va = eval_expr env a in
        (match vf with
        | VClos { name = _; arg = x; body; env = clo_env } -> 
            eval_expr (Env.add x va clo_env) body
        | _ -> raise CompareFunVals)
    | Let { is_rec = false; name; binding; body } -> 
        let bv = eval_expr env binding in 
        eval_expr (Env.add name bv env) body
    | Let { is_rec = true; name; binding = Fun (x, _, b); body } ->
        (* Create a reference cell for the recursive environment *)
        let rec_env = ref env in
        let closure = VClos { name = Some name; arg = x; body = b; env = !rec_env } in
        rec_env := Env.add name closure env;
        eval_expr (Env.add name closure env) body
    | Let _ -> failwith "Recursion for non-function not supported"
    | Annot (e, _) -> eval_expr env e
    | Assert e -> 
        (match eval_expr env e with 
        | VBool true -> VUnit 
        | VBool false -> raise AssertFail 
        | _ -> failwith "Invalid assertion, expected boolean")
    | If (c, t, e) -> 
        (match eval_expr env c with 
        | VBool true -> eval_expr env t 
        | VBool false -> eval_expr env e 
        | _ -> failwith "If condition must be boolean")
    | Nil -> VList []
    | ENone -> VNone
    | ESome e -> VSome (eval_expr env e)
    | Bop (op, e1, e2) ->
        let v1 = eval_expr env e1 in
        let v2 = eval_expr env e2 in
        (match op with
        | Add -> (match v1, v2 with VInt a, VInt b -> VInt (a + b) | _ -> failwith "Expected ints")
        | Sub -> (match v1, v2 with VInt a, VInt b -> VInt (a - b) | _ -> failwith "Expected ints")
        | Mul -> (match v1, v2 with VInt a, VInt b -> VInt (a * b) | _ -> failwith "Expected ints")
        | Div -> (match v1, v2 with VInt a, VInt b -> 
                  if b = 0 then raise DivByZero else VInt (a / b) | _ -> failwith "Expected ints")
        | Mod -> (match v1, v2 with VInt a, VInt b -> 
                  if b = 0 then raise DivByZero else VInt (a mod b) | _ -> failwith "Expected ints")
        | AddF -> (match v1, v2 with VFloat a, VFloat b -> VFloat (a +. b) | _ -> failwith "Expected floats")
        | SubF -> (match v1, v2 with VFloat a, VFloat b -> VFloat (a -. b) | _ -> failwith "Expected floats")
        | MulF -> (match v1, v2 with VFloat a, VFloat b -> VFloat (a *. b) | _ -> failwith "Expected floats")
        | DivF -> (match v1, v2 with VFloat a, VFloat b -> VFloat (a /. b) | _ -> failwith "Expected floats")
        | PowF -> (match v1, v2 with VFloat a, VFloat b -> VFloat (a ** b) | _ -> failwith "Expected floats")
        | Lt -> (match v1, v2 with 
                 | VInt a, VInt b -> VBool (a < b) 
                 | _ -> failwith "Expected comparable values")
        | Lte -> (match v1, v2 with 
                  | VInt a, VInt b -> VBool (a <= b) 
                  | _ -> failwith "Expected comparable values")
        | Gt -> (match v1, v2 with 
                 | VInt a, VInt b -> VBool (a > b) 
                 | _ -> failwith "Expected comparable values")
        | Gte -> (match v1, v2 with 
                  | VInt a, VInt b -> VBool (a >= b) 
                  | _ -> failwith "Expected comparable values")
        | Eq -> (try VBool (v1 = v2) with Invalid_argument _ -> raise CompareFunVals)
        | Neq -> (try VBool (v1 <> v2) with Invalid_argument _ -> raise CompareFunVals)
        | And -> (match v1, v2 with VBool a, VBool b -> VBool (a && b) | _ -> failwith "Expected booleans")
        | Or -> (match v1, v2 with VBool a, VBool b -> VBool (a || b) | _ -> failwith "Expected booleans")
        | Comma -> VPair (v1, v2)
        | Cons -> (match v2 with VList l -> VList (v1 :: l) | _ -> failwith "Expected list"))
    | ListMatch { matched; hd_name; tl_name; cons_case; nil_case } ->
        (match eval_expr env matched with
        | VList [] -> eval_expr env nil_case
        | VList (h :: t) -> eval_expr (Env.add hd_name h (Env.add tl_name (VList t) env)) cons_case
        | _ -> failwith "Expected list")
    | OptMatch { matched; some_name; some_case; none_case } ->
        (match eval_expr env matched with
        | VSome v -> eval_expr (Env.add some_name v env) some_case
        | VNone -> eval_expr env none_case
        | _ -> failwith "Expected option")
    | PairMatch { matched; fst_name; snd_name; case } ->
        (match eval_expr env matched with
        | VPair (v1, v2) -> eval_expr (Env.add fst_name v1 (Env.add snd_name v2 env)) case
        | _ -> failwith "Expected pair")

let eval p =
  let rec nest = function
    | [] -> Unit
    | [{is_rec;name;binding}] -> Let {is_rec;name;binding;body = Var name}
    | {is_rec;name;binding} :: ls -> Let {is_rec;name;binding;body = nest ls}
  in eval_expr Env.empty (nest p)
        

let interp input =
  match parse input with
  | Some prog ->
    if is_well_typed prog
    then Ok (eval prog)
    else Error TypeError
  | None -> Error ParseError