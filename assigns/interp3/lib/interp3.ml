include Utils

let parse (s : string) : prog option =
  match Parser.prog Lexer.read (Lexing.from_string s) with
  | prog -> Some prog
  | exception _ -> None

(* Apply a substitution to a type *)
let rec apply_subst (subst : (string * ty) list) (t : ty) : ty =
  match t with
  | TUnit | TInt | TFloat | TBool -> t
  | TVar x -> 
      (match List.assoc_opt x subst with
       | Some ty -> ty
       | None -> TVar x)
  | TFun (t1, t2) -> TFun (apply_subst subst t1, apply_subst subst t2)
  | TPair (t1, t2) -> TPair (apply_subst subst t1, apply_subst subst t2)
  | TList t -> TList (apply_subst subst t)
  | TOption t -> TOption (apply_subst subst t)

(* Check if a type variable appears in a type *)
let rec occurs (x : string) (t : ty) : bool =
  match t with
  | TUnit | TInt | TFloat | TBool -> false
  | TVar y -> x = y
  | TFun (t1, t2) -> occurs x t1 || occurs x t2
  | TPair (t1, t2) -> occurs x t1 || occurs x t2
  | TList t -> occurs x t
  | TOption t -> occurs x t

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
               Some (subst2 @ subst1))
               
  | TPair (s1, s2), TPair (t1, t2) ->
      (match unify_one s1 t1 with
       | None -> None
       | Some subst1 ->
           let s2' = apply_subst subst1 s2 in
           let t2' = apply_subst subst1 t2 in
           match unify_one s2' t2' with
           | None -> None
           | Some subst2 -> 
               Some (subst2 @ subst1))
               
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
          let rest' = List.map (fun (t1, t2) -> 
                              (apply_subst subst1 t1, apply_subst subst1 t2)) rest in
          match unify rest' with
          | None -> None
          | Some subst2 -> 
              Some (subst2 @ subst1)

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

let type_of (_ctxt: stc_env) (_e : expr) : ty_scheme option = assert false

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