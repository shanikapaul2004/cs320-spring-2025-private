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

(* Substitute a type for a type variable in another type *)
let rec ty_subst (new_ty : ty) (var : string) (target : ty) : ty =
  match target with
  | TUnit | TInt | TFloat | TBool -> target
  | TVar x -> if x = var then new_ty else target
  | TFun (t1, t2) -> TFun (ty_subst new_ty var t1, ty_subst new_ty var t2)
  | TPair (t1, t2) -> TPair (ty_subst new_ty var t1, ty_subst new_ty var t2)
  | TList t -> TList (ty_subst new_ty var t)
  | TOption t -> TOption (ty_subst new_ty var t)

let instantiate (Forall (vars, ty)) =
  let subst = VarSet.fold
    (fun var subst ->
      let fresh_var = TVar (gensym ()) in
      (var, fresh_var) :: subst)
    vars
    []
  in
    (* Apply the substitution to the type *)
  List.fold_left
    (fun ty (var, fresh_ty) -> apply_subst [(var, fresh_ty)] ty)
    ty
    subst

(* Generalize a type to a type scheme by quantifying free type variables
   that don't appear in the context *)
   let generalize (env : stc_env) (ty : ty) : ty_scheme =
    (* Get free variables in the environment *)
    let env_vars = Env.fold
      (fun _ scheme acc ->
        let (Forall (_, t)) = scheme in
        VarSet.union (free_vars_ty t) acc)
      env
      VarSet.empty
    in
    
    (* Get free variables in the type that aren't in the environment *)
    let free = VarSet.diff (free_vars_ty ty) env_vars in
    
    Forall (free, ty)
  

let rec infer_expr (env : stc_env) (e : expr) : (ty * constr list) option= 
  match e with
  | Unit -> Some (TUnit, [])
  | Bool _ -> Some(TBool, [])
  | Int _ -> Some(TInt, [])
  | Float _ -> Some(TFloat, [])
  | Var x -> (match Env.find_opt x env with 
    | Some scheme -> let ty = instantiate scheme in
        Some (ty, [])
    | None -> None)
  | ENone -> (let a = TVar (gensym ()) in
      Some (TOption a, []))
  | ESome e -> (match infer_expr env e with
     | Some (t, c) -> Some (TOption t, c)
     | None -> None)
  | Nil -> (let a = TVar (gensym ()) in
      Some (TList a, []))
  | Bop (op, e1, e2) -> (match infer_expr env e1, infer_expr env e2 with
       | Some (t1, c1), Some (t2, c2) ->
           begin match op with
           | Add | Sub | Mul | Div | Mod ->
               Some (TInt, (t1, TInt) :: (t2, TInt) :: (c1 @ c2))
          | AddF | SubF | MulF | DivF | PowF ->
              Some (TFloat, (t1, TFloat) :: (t2, TFloat) :: (c1 @ c2))
           | Eq | Neq | Lt | Lte | Gt | Gte ->
               Some (TBool, (t1, t2) :: (c1 @ c2))
           | And | Or ->
               Some (TBool, (t1, TBool) :: (t2, TBool) :: (c1 @ c2))
           | Cons ->
               let a = TVar (gensym ()) in
               Some (TList a, (t1, a) :: (t2, TList a) :: (c1 @ c2))
           | Comma ->
               Some (TPair (t1, t2), c1 @ c2)
           end
       | _ -> None)
  | If (e1, e2, e3) -> 
      (match infer_expr env e1, infer_expr env e2, infer_expr env e3 with
      | Some (t1, c1), Some (t2, c2), Some (t3, c3) ->
          Some (t3, (t1, TBool) :: (t2, t3) :: (c1 @ c2 @ c3))
      | _ -> None)
  | Assert e -> (if e = Bool false then let a = TVar (gensym ()) in Some (a,[]) 
      else match infer_expr env e with
      | Some (t, c) -> Some (TUnit, (t, TBool) :: c)
      | None -> None)
  | Annot (e, t_expected) ->
      (match infer_expr env e with
      | Some (t, c) -> Some (t_expected, (t, t_expected) :: c)
      | None -> None)
  | Fun (x, None, body) ->
      let a = TVar (gensym ()) in
      let env' = Env.add x (Forall (VarSet.empty, a)) env in
      (match infer_expr env' body with
        | Some (t_body, c_body) -> Some (TFun (a, t_body), c_body)
        | None -> None)
  | Fun (x, Some ty, body) ->
      let env' = Env.add x (Forall (VarSet.empty, ty)) env in
      (match infer_expr env' body with
        | Some (t_body, c_body) -> Some (TFun (ty, t_body), c_body)
        | None -> None)
  | App (e1, e2) ->
      (match infer_expr env e1, infer_expr env e2 with
        | Some (t1, c1), Some (t2, c2) ->
            let a = TVar (gensym ()) in
            Some (a, (t1, TFun (t2, a)) :: (c1 @ c2))
        | _ -> None)
  | Let { is_rec = false; name; binding; body } -> (
    match infer_expr env binding with
    | Some (t1, c1) ->
        let env' = Env.add name (Forall (VarSet.empty, t1)) env in
        (match infer_expr env' body with
          | Some (t2, c2) -> Some (t2, c1 @ c2)
          | None -> None)
    | None -> None)
   | Let { is_rec = true; name; binding; body } -> (
    match binding with
    | Fun _ ->
        let a = TVar (gensym ()) in
        let env' = Env.add name (Forall (VarSet.empty, a)) env in
        (match infer_expr env' binding with
          | Some (t1, c1) ->
              (match infer_expr env' body with
              | Some (t2, c2) ->
                  Some (t2, (t1, a) :: (c1 @ c2))
              | None -> None)
          | None -> None)
    | _ -> None)
  | ListMatch { matched; hd_name; tl_name; cons_case; nil_case } ->
      (match infer_expr env matched with
        | Some (t_match, c_match) ->
            let a = TVar (gensym ()) in
            let env_cons = env
              |> Env.add hd_name (Forall (VarSet.empty, a))
              |> Env.add tl_name (Forall (VarSet.empty, TList a)) in
            (match infer_expr env_cons cons_case, infer_expr env nil_case with
            | Some (t1, c1), Some (t2, c2) ->
              let r = TVar (gensym ()) in
              Some (r, (t_match, TList a) :: (t1, r) :: (t2, r) :: (c_match @ c1 @ c2)) 
            | _ -> None)
        | None -> None)
  | OptMatch { matched; some_name; some_case; none_case } ->
      (match infer_expr env matched with
        | Some (t_match, c_match) ->
            let a = TVar (gensym ()) in
            let env_some = Env.add some_name (Forall (VarSet.empty, a)) env in
            (match infer_expr env_some some_case, infer_expr env none_case with
            | Some (t1, c1), Some (t2, c2) ->
              Some (t2, (t_match, TOption a) :: (t1, t2) :: (c_match @ c1 @ c2))
            | _ -> None)
        | None -> None)
  | PairMatch { matched; fst_name; snd_name; case } ->
      (match infer_expr env matched with
        | Some (t_match, c_match) ->
            let a = TVar (gensym ()) in
            let b = TVar (gensym ()) in
            let env' =
              env
              |> Env.add fst_name (Forall (VarSet.empty, a))
              |> Env.add snd_name (Forall (VarSet.empty, b)) in
            (match infer_expr env' case with
            | Some (t_body, c_body) ->
                Some (t_body, (t_match, TPair (a, b)) :: (c_match @ c_body))
            | None -> None)
        | None -> None)
    
let type_of (ctxt: stc_env) (e : expr) : ty_scheme option = 
  match infer_expr ctxt e with
  | None -> None
  | Some (t, cs) ->
      match unify cs with
      | None -> None
      | Some subst ->
          let t' = List.fold_left (fun ty (x, ty') -> ty_subst ty' x ty) t subst in
          Some (generalize ctxt t')

(*type of function end *)


let is_well_typed (_p : prog) : bool = assert false

exception AssertFail
exception DivByZero
exception CompareFunVals

let eval_expr (_env : dyn_env) (_e : expr) : value = assert false

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