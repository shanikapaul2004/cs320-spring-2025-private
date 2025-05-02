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

(* Type substitution function - replaces type variables with types in a target type *)
let rec ty_replace (replacement : ty) (var_name : string) (in_type : ty) : ty =
  match in_type with
  | TUnit | TInt | TFloat | TBool -> 
      (* Base types remain unchanged *)
      in_type
  | TVar tv_name -> 
      (* Replace variable if it matches the target *)
      if tv_name = var_name then replacement else in_type
  | TFun (param_ty, return_ty) -> 
      (* Recursively substitute in both parts of a function type *)
      TFun (ty_replace replacement var_name param_ty, 
            ty_replace replacement var_name return_ty)
  | TPair (left_ty, right_ty) -> 
      (* Recursively substitute in both parts of a pair type *)
      TPair (ty_replace replacement var_name left_ty, 
             ty_replace replacement var_name right_ty)
  | TList element_ty -> 
      (* Recursively substitute in element type *)
      TList (ty_replace replacement var_name element_ty)
  | TOption wrapped_ty -> 
      (* Recursively substitute in wrapped type *)
      TOption (ty_replace replacement var_name wrapped_ty)

(* Create concrete instance from polymorphic type schema *)
let make_concrete (Forall (quantified_vars, poly_type)) =
  (* Create a fresh type variable for each quantified variable *)
  let variable_mappings = VarSet.fold
    (fun var_name mappings_acc ->
      let fresh_type_var = TVar (gensym ()) in
      (var_name, fresh_type_var) :: mappings_acc)
    quantified_vars
    []
  in
  
  (* Apply all substitutions to get a fresh concrete type *)
  List.fold_left
    (fun current_type (var_name, new_var) -> 
       apply_subst [(var_name, new_var)] current_type)
    poly_type
    variable_mappings

(* Convert monomorphic type to a type scheme by quantifying appropriate variables *)
let make_polymorphic (context : stc_env) (monomorphic_type : ty) : ty_scheme =
  (* Collect all free type variables from the environment *)
  let env_type_vars = Env.fold
    (fun _ scheme acc_vars ->
      let (Forall (_, scheme_type)) = scheme in
      VarSet.union (free_vars_ty scheme_type) acc_vars)
    context
    VarSet.empty
  in
  
  (* Find variables that appear in the type but not in the environment context *)
  let quantifiable_vars = VarSet.diff (free_vars_ty monomorphic_type) env_type_vars in
  
  (* Create polymorphic scheme with appropriate quantification *)
  Forall (quantifiable_vars, monomorphic_type)

(* Main type inference algorithm *)
let rec analyze_type (ctx : stc_env) (expression : expr) : (ty * constr list) option = 
  match expression with
  (* Basic literals *)
  | Unit -> 
      Some (TUnit, [])
  | Bool _ -> 
      Some (TBool, [])
  | Int _ -> 
      Some (TInt, [])
  | Float _ -> 
      Some (TFloat, [])
      
  (* Variable reference - look up in context *)
  | Var name -> 
      (match Env.find_opt name ctx with 
       | Some type_scheme -> 
           let concrete_type = make_concrete type_scheme in
           Some (concrete_type, [])
       | None -> None)
       
  (* Option constructors *)
  | ENone -> 
      let element_type = TVar (gensym ()) in
      Some (TOption element_type, [])
  | ESome inner_expr -> 
      (match analyze_type ctx inner_expr with
       | Some (inner_type, constraints) -> 
           Some (TOption inner_type, constraints)
       | None -> None)
       
  (* Empty list constructor *)
  | Nil -> 
      let element_type = TVar (gensym ()) in
      Some (TList element_type, [])
      
  (* Binary operators *)
  | Bop (operator, left_expr, right_expr) -> 
      (match analyze_type ctx left_expr, analyze_type ctx right_expr with
       | Some (left_type, left_constraints), Some (right_type, right_constraints) ->
           begin match operator with
           (* Integer arithmetic *)
           | Add | Sub | Mul | Div | Mod ->
               Some (TInt, 
                    (left_type, TInt) :: (right_type, TInt) :: 
                    (left_constraints @ right_constraints))
           (* Floating-point arithmetic *)
           | AddF | SubF | MulF | DivF | PowF ->
               Some (TFloat, 
                    (left_type, TFloat) :: (right_type, TFloat) :: 
                    (left_constraints @ right_constraints))
           (* Comparison operators *)
           | Eq | Neq | Lt | Lte | Gt | Gte ->
               Some (TBool, 
                    (left_type, right_type) :: 
                    (left_constraints @ right_constraints))
           (* Logical operators *)
           | And | Or ->
               Some (TBool, 
                    (left_type, TBool) :: (right_type, TBool) :: 
                    (left_constraints @ right_constraints))
           (* List construction *)
           | Cons ->
               let element_type = TVar (gensym ()) in
               Some (TList element_type, 
                    (left_type, element_type) :: 
                    (right_type, TList element_type) :: 
                    (left_constraints @ right_constraints))
           (* Tuple construction *)
           | Comma ->
               Some (TPair (left_type, right_type), 
                    left_constraints @ right_constraints)
           end
       | _ -> None)
       
  (* Conditional expression *)
  | If (condition, then_branch, else_branch) -> 
      (match analyze_type ctx condition, 
             analyze_type ctx then_branch, 
             analyze_type ctx else_branch with
       | Some (cond_type, cond_constraints), 
         Some (then_type, then_constraints), 
         Some (else_type, else_constraints) ->
           Some (else_type, 
                (cond_type, TBool) :: (then_type, else_type) :: 
                (cond_constraints @ then_constraints @ else_constraints))
       | _ -> None)
       
  (* Assertions *)
  | Assert assertion_expr -> 
      if assertion_expr = Bool false then 
        (* assert false can have any type *)
        let result_type = TVar (gensym ()) in 
        Some (result_type, []) 
      else 
        match analyze_type ctx assertion_expr with
        | Some (assertion_type, assertion_constraints) -> 
            Some (TUnit, (assertion_type, TBool) :: assertion_constraints)
        | None -> None
        
  (* Type annotation *)
  | Annot (inner_expr, expected_type) ->
      (match analyze_type ctx inner_expr with
       | Some (inferred_type, constraints) -> 
           Some (expected_type, (inferred_type, expected_type) :: constraints)
       | None -> None)
       
  (* Function abstraction *)
  | Fun (param_name, param_type_opt, body_expr) ->
      match param_type_opt with
      | None ->
          (* Unannotated parameter - generate fresh type variable *)
          let param_type = TVar (gensym ()) in
          let extended_ctx = Env.add param_name (Forall (VarSet.empty, param_type)) ctx in
          (match analyze_type extended_ctx body_expr with
           | Some (body_type, body_constraints) -> 
               Some (TFun (param_type, body_type), body_constraints)
           | None -> None)
      | Some explicit_type ->
          (* Use explicitly provided parameter type *)
          let extended_ctx = Env.add param_name (Forall (VarSet.empty, explicit_type)) ctx in
          (match analyze_type extended_ctx body_expr with
           | Some (body_type, body_constraints) -> 
               Some (TFun (explicit_type, body_type), body_constraints)
           | None -> None)
           
  (* Function application *)
  | App (func_expr, arg_expr) ->
      (match analyze_type ctx func_expr, analyze_type ctx arg_expr with
       | Some (func_type, func_constraints), Some (arg_type, arg_constraints) ->
           let result_type = TVar (gensym ()) in
           Some (result_type, 
                (func_type, TFun (arg_type, result_type)) :: 
                (func_constraints @ arg_constraints))
       | _ -> None)
       
  (* Non-recursive let binding *)
  | Let { is_rec = false; name; binding; body } -> 
      (match analyze_type ctx binding with
       | Some (binding_type, binding_constraints) ->
           let extended_ctx = Env.add name (Forall (VarSet.empty, binding_type)) ctx in
           (match analyze_type extended_ctx body with
            | Some (body_type, body_constraints) -> 
                Some (body_type, binding_constraints @ body_constraints)
            | None -> None)
       | None -> None)
       
  (* Recursive let binding *)
  | Let { is_rec = true; name; binding; body } -> 
      match binding with
      | Fun _ ->
          (* Create a placeholder type for the recursive function *)
          let rec_type = TVar (gensym ()) in
          let extended_ctx = Env.add name (Forall (VarSet.empty, rec_type)) ctx in
          (match analyze_type extended_ctx binding with
           | Some (binding_type, binding_constraints) ->
               (match analyze_type extended_ctx body with
                | Some (body_type, body_constraints) ->
                    Some (body_type, 
                         (binding_type, rec_type) :: 
                         (binding_constraints @ body_constraints))
                | None -> None)
           | None -> None)
      | _ -> None  (* Only functions can be recursive *)
      
  (* List pattern matching *)
  | ListMatch { matched; hd_name; tl_name; cons_case; nil_case } ->
      (match analyze_type ctx matched with
       | Some (matched_type, matched_constraints) ->
           let element_type = TVar (gensym ()) in
           let cons_ctx = ctx
             |> Env.add hd_name (Forall (VarSet.empty, element_type))
             |> Env.add tl_name (Forall (VarSet.empty, TList element_type)) in
           (match analyze_type cons_ctx cons_case, analyze_type ctx nil_case with
            | Some (cons_type, cons_constraints), Some (nil_type, nil_constraints) ->
                let result_type = TVar (gensym ()) in
                Some (result_type, 
                     (matched_type, TList element_type) :: 
                     (cons_type, result_type) :: 
                     (nil_type, result_type) :: 
                     (matched_constraints @ cons_constraints @ nil_constraints)) 
            | _ -> None)
       | None -> None)
       
  (* Option pattern matching *)
  | OptMatch { matched; some_name; some_case; none_case } ->
      (match analyze_type ctx matched with
       | Some (matched_type, matched_constraints) ->
           let inner_type = TVar (gensym ()) in
           let some_ctx = Env.add some_name (Forall (VarSet.empty, inner_type)) ctx in
           (match analyze_type some_ctx some_case, analyze_type ctx none_case with
            | Some (some_type, some_constraints), Some (none_type, none_constraints) ->
                Some (none_type, 
                     (matched_type, TOption inner_type) :: 
                     (some_type, none_type) :: 
                     (matched_constraints @ some_constraints @ none_constraints))
            | _ -> None)
       | None -> None)
       
  (* Pair pattern matching *)
  | PairMatch { matched; fst_name; snd_name; case } ->
      (match analyze_type ctx matched with
       | Some (matched_type, matched_constraints) ->
           let first_type = TVar (gensym ()) in
           let second_type = TVar (gensym ()) in
           let pair_ctx =
             ctx
             |> Env.add fst_name (Forall (VarSet.empty, first_type))
             |> Env.add snd_name (Forall (VarSet.empty, second_type)) in
           (match analyze_type pair_ctx case with
            | Some (case_type, case_constraints) ->
                Some (case_type, 
                     (matched_type, TPair (first_type, second_type)) :: 
                     (matched_constraints @ case_constraints))
            | None -> None)
       | None -> None)

(* Main type inference function *)
let derive_type_scheme (context: stc_env) (expression : expr) : ty_scheme option = 
  match analyze_type context expression with
  | None -> 
      (* Type inference failed *)
      None
  | Some (inferred_type, constraints) ->
      (* Try to solve the collected type constraints *)
      match unify constraints with
      | None -> 
          (* No solution found - expression is ill-typed *)
          None
      | Some substitutions ->
          (* Apply substitutions to get the final concrete type *)
          let final_type = List.fold_left 
            (fun current_ty (var_name, replacement_ty) -> 
              ty_replace replacement_ty var_name current_ty) 
            inferred_type 
            substitutions 
          in
          (* Generalize the type by quantifying appropriate variables *)
          Some (make_polymorphic context final_type)

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