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

let type_of (ctxt: stc_env) (e : expr) : ty_scheme option =
  (* Helper to infer type and collect constraints *)
  let rec infer (env: stc_env) (e: expr) : (ty * constr list) =
    match e with
    | Unit -> (TUnit, [])
    | Bool _ -> (TBool, [])
    | Int _ -> (TInt, [])
    | Float _ -> (TFloat, [])
    | Nil -> 
        let alpha = TVar (gensym ()) in
        (TList alpha, [])
    | ENone -> 
        let alpha = TVar (gensym ()) in
        (TOption alpha, [])
    | ESome e ->
        let (t, c) = infer env e in
        (TOption t, c)
    | Var x ->
        (match Env.find_opt x env with
         | Some scheme -> 
             (* Instantiate type scheme with fresh variables *)
             let Forall (vars, ty) = scheme in
             let subst = VarSet.fold
               (fun v acc -> (v, TVar (gensym ())) :: acc)
               vars
               [] in
             (apply_subst subst ty, [])
         | None -> failwith ("Unbound variable: " ^ x))
    | Bop (op, e1, e2) ->
        let (t1, c1) = infer env e1 in
        let (t2, c2) = infer env e2 in
        (match op with
         | Add | Sub | Mul | Div | Mod -> 
             (TInt, c1 @ c2 @ [(t1, TInt); (t2, TInt)])
         | AddF | SubF | MulF | DivF | PowF -> 
             (TFloat, c1 @ c2 @ [(t1, TFloat); (t2, TFloat)])
         | Lt | Lte | Gt | Gte | Eq | Neq -> 
             (TBool, c1 @ c2 @ [(t1, t2)])
         | And | Or -> 
             (TBool, c1 @ c2 @ [(t1, TBool); (t2, TBool)])
         | Comma -> 
             (TPair (t1, t2), c1 @ c2)
         | Cons -> 
             (t1, c1 @ c2 @ [(t2, TList t1); (t1, t1)]))
    | If (e1, e2, e3) ->
        let (t1, c1) = infer env e1 in
        let (t2, c2) = infer env e2 in
        let (t3, c3) = infer env e3 in
        (t2, c1 @ c2 @ c3 @ [(t1, TBool); (t2, t3)])
    | Fun (x, ty_opt, body) ->
        (match ty_opt with
         | Some param_ty ->
             let param_env = Env.add x (Forall (VarSet.empty, param_ty)) env in
             let (return_ty, c) = infer param_env body in
             (TFun (param_ty, return_ty), c)
         | None ->
             let param_ty = TVar (gensym ()) in
             let param_env = Env.add x (Forall (VarSet.empty, param_ty)) env in
             let (return_ty, c) = infer param_env body in
             (TFun (param_ty, return_ty), c))
    | App (e1, e2) ->
        let (t1, c1) = infer env e1 in
        let (t2, c2) = infer env e2 in
        let alpha = TVar (gensym ()) in
        (alpha, c1 @ c2 @ [(t1, TFun (t2, alpha))])
    | Annot (e, ty) ->
        let (t, c) = infer env e in
        (ty, c @ [(t, ty)])
    | Assert e ->
        (match e with
         | Bool false -> 
             let alpha = TVar (gensym ()) in
             (alpha, [])
         | _ ->
             let (t, c) = infer env e in
             (TUnit, c @ [(t, TBool)]))
    | Let { is_rec; name; binding; body } ->
        if is_rec then
          (* For recursive let-bindings *)
          let alpha = TVar (gensym ()) in
          let temp_env = Env.add name (Forall (VarSet.empty, alpha)) env in
          let (binding_ty, binding_c) = infer temp_env binding in
          
          (* Check if binding is an anonymous function *)
          let is_fun = match binding with
                       | Fun (_, _, _) -> true
                       | _ -> false in
          
          if not is_fun then
            failwith "Recursive binding must be a function"
          else
            let constraints = binding_c @ [(alpha, binding_ty)] in
            (match unify constraints with
             | None -> failwith "Cannot unify constraints in recursive binding"
             | Some subst ->
                 let resolved_ty = apply_subst subst binding_ty in
                 
                 (* Find free type variables in the environment *)
                 let free_in_env = Env.fold 
                   (fun _ scheme acc -> 
                     match scheme with
                     | Forall (_, t) -> VarSet.union acc (free_vars_ty t))
                   env
                   VarSet.empty in
                 
                 (* Calculate which variables should be generalized *)
                 let gen_vars = VarSet.diff (free_vars_ty resolved_ty) free_in_env in
                 
                 let binding_scheme = Forall (gen_vars, resolved_ty) in
                 let new_env = Env.add name binding_scheme env in
                 let (body_ty, body_c) = infer new_env body in
                 (body_ty, binding_c @ body_c))
        else
          (* For non-recursive let-bindings *)
          let (binding_ty, binding_c) = infer env binding in
          (match unify binding_c with
           | None -> failwith "Cannot unify constraints in binding"
           | Some subst ->
               let resolved_ty = apply_subst subst binding_ty in
               
               (* Find free type variables in the environment *)
               let free_in_env = Env.fold 
                 (fun _ scheme acc -> 
                   match scheme with
                   | Forall (_, t) -> VarSet.union acc (free_vars_ty t))
                 env
                 VarSet.empty in
               
               (* Calculate which variables should be generalized *)
               let gen_vars = VarSet.diff (free_vars_ty resolved_ty) free_in_env in
               
               let binding_scheme = Forall (gen_vars, resolved_ty) in
               let new_env = Env.add name binding_scheme env in
               let (body_ty, body_c) = infer new_env body in
               (body_ty, binding_c @ body_c))
    | ListMatch { matched; hd_name; tl_name; cons_case; nil_case } ->
        let (matched_ty, matched_c) = infer env matched in
        let element_ty = TVar (gensym ()) in
        let list_ty = TList element_ty in
        
        let extended_env = Env.add hd_name (Forall (VarSet.empty, element_ty))
                           (Env.add tl_name (Forall (VarSet.empty, list_ty)) env) in
        
        let (cons_ty, cons_c) = infer extended_env cons_case in
        let (nil_ty, nil_c) = infer env nil_case in
        
        (nil_ty, matched_c @ cons_c @ nil_c @ [(matched_ty, list_ty); (cons_ty, nil_ty)])
    | OptMatch { matched; some_name; some_case; none_case } ->
        let (matched_ty, matched_c) = infer env matched in
        let element_ty = TVar (gensym ()) in
        let opt_ty = TOption element_ty in
        
        let extended_env = Env.add some_name (Forall (VarSet.empty, element_ty)) env in
        let (some_ty, some_c) = infer extended_env some_case in
        let (none_ty, none_c) = infer env none_case in
        
        (some_ty, matched_c @ some_c @ none_c @ [(matched_ty, opt_ty); (some_ty, none_ty)])
    | PairMatch { matched; fst_name; snd_name; case } ->
        let (matched_ty, matched_c) = infer env matched in
        let alpha = TVar (gensym ()) in
        let beta = TVar (gensym ()) in
        let pair_ty = TPair (alpha, beta) in
        
        let extended_env = Env.add fst_name (Forall (VarSet.empty, alpha))
                          (Env.add snd_name (Forall (VarSet.empty, beta)) env) in
        
        let (case_ty, case_c) = infer extended_env case in
        (case_ty, matched_c @ case_c @ [(matched_ty, pair_ty)])
  in
  
  try
    let (ty, constraints) = infer ctxt e in
    principle_type ty constraints
  with
  | Failure _ -> None

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