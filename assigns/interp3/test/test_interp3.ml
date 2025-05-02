(* test_interp3.ml - Testing file for interp3 *)
open Interp3

(* Add counters for tracking passed and failed tests *)
let passed = ref 0
let failed = ref 0

(* Helper function to convert a list of bindings to a static environment *)
let ctx_to_env ctx =
  List.fold_left 
    (fun env (name, ty_scheme) -> Env.add name ty_scheme env) 
    Env.empty 
    ctx

(* Helper function to convert types to strings *)
let rec string_of_ty = function
  | TUnit -> "TUnit"
  | TInt -> "TInt"
  | TFloat -> "TFloat"
  | TBool -> "TBool"
  | TVar s -> "TVar(" ^ s ^ ")"
  | TFun (t1, t2) -> "TFun(" ^ string_of_ty t1 ^ ", " ^ string_of_ty t2 ^ ")"
  | TPair (t1, t2) -> "TPair(" ^ string_of_ty t1 ^ ", " ^ string_of_ty t2 ^ ")"
  | TList t -> "TList(" ^ string_of_ty t ^ ")"
  | TOption t -> "TOption(" ^ string_of_ty t ^ ")"

(* Helper function to test type_of on expressions *)
let test_type_of ?(ctx = []) name expr expected =
  let env = ctx_to_env ctx in
  match type_of env expr with
  | Some (Forall (_, ty)) when ty = expected ->
      incr passed;
      Printf.printf "PASS: %s => %s\n" name (string_of_ty ty)
  | Some (Forall (_, ty)) ->
      incr failed;
      Printf.printf "FAIL: %s => Got %s, Expected %s\n" name (string_of_ty ty) (string_of_ty expected)
  | None ->
      incr failed;
      Printf.printf "FAIL: %s => Type inference failed\n" name

(* Helper function to convert constraints to strings *)
let string_of_constr (t1, t2) =
  "(" ^ string_of_ty t1 ^ ", " ^ string_of_ty t2 ^ ")"

let string_of_constrs cs =
  "[" ^ (String.concat "; " (List.map string_of_constr cs)) ^ "]"

let print_ty_scheme = function
  | None -> "None"
  | Some (Forall (vars, ty)) -> 
      "Forall ({" ^ (String.concat "; " (VarSet.elements vars)) ^ "}, " ^ (string_of_ty ty) ^ ")"

let test_principle_type ty cs expected_result =
  let result = principle_type ty cs in
  let result_str = print_ty_scheme result in
  let expected_str = print_ty_scheme expected_result in
  if result_str = expected_str then (
    incr passed;
    Printf.printf "PASS: principle_type %s %s = %s\n" 
      (string_of_ty ty) 
      (string_of_constrs cs) 
      result_str
  ) else (
    incr failed;
    Printf.printf "FAIL: principle_type %s %s\n  Expected: %s\n  Got: %s\n" 
      (string_of_ty ty) 
      (string_of_constrs cs) 
      expected_str 
      result_str
  )

(* Helper functions to create types and constraints *)
let int_ty = TInt
let bool_ty = TBool
let var_ty s = TVar s
let fun_ty t1 t2 = TFun (t1, t2)
let list_ty t = TList t
let option_ty t = TOption t
let pair_ty t1 t2 = TPair (t1, t2)

let teq t1 t2 = (t1, t2)  (* Assuming constr is defined as ty * ty *)

(* Basic tests *)
let test1 () =
  let ty = int_ty in
  let cs = [] in
  test_principle_type ty cs (Some (Forall (VarSet.empty, int_ty)));
  
  let ty = var_ty "a" in
  let cs = [teq (var_ty "a") int_ty] in
  test_principle_type ty cs (Some (Forall (VarSet.empty, int_ty)))

(* Function type tests *)
let test2 () =
  let ty = fun_ty (var_ty "a") (var_ty "b") in
  let cs = [teq (var_ty "a") int_ty; teq (var_ty "b") bool_ty] in
  test_principle_type ty cs (Some (Forall (VarSet.empty, (fun_ty int_ty bool_ty))));
  
  let ty = var_ty "c" in
  let cs = [teq (var_ty "c") (fun_ty (var_ty "a") (var_ty "b")); 
            teq (var_ty "a") int_ty; 
            teq (var_ty "b") bool_ty] in
  test_principle_type ty cs (Some (Forall (VarSet.empty, (fun_ty int_ty bool_ty))))

(* Polymorphic type tests *)
let test3 () =
  let ty = fun_ty (var_ty "a") (var_ty "a") in
  let cs = [] in
  let vars = VarSet.singleton "a" in
  test_principle_type ty cs (Some (Forall (vars, fun_ty (var_ty "a") (var_ty "a"))));
  
  let ty = fun_ty (list_ty (var_ty "a")) (var_ty "a") in
  let cs = [] in
  let vars = VarSet.singleton "a" in
  test_principle_type ty cs (Some (Forall (vars, fun_ty (list_ty (var_ty "a")) (var_ty "a"))))

(* Unification failure tests *)
let test4 () =
  let ty = var_ty "a" in
  let cs = [teq (var_ty "a") int_ty; teq (var_ty "a") bool_ty] in
  test_principle_type ty cs None;
  
  let ty = var_ty "a" in
  let cs = [teq (var_ty "a") (fun_ty (var_ty "a") int_ty)] in
  test_principle_type ty cs None (* Occurs check failure *)

(* List and option type tests *)
let test5 () =
  let ty = list_ty (var_ty "a") in
  let cs = [teq (var_ty "a") int_ty] in
  test_principle_type ty cs (Some (Forall (VarSet.empty, (list_ty int_ty))));
  
  let ty = option_ty (var_ty "a") in
  let cs = [teq (var_ty "a") bool_ty] in
  test_principle_type ty cs (Some (Forall (VarSet.empty, (option_ty bool_ty))))

(* Pair type tests *)
let test6 () =
  let ty = pair_ty (var_ty "a") (var_ty "b") in
  let cs = [teq (var_ty "a") int_ty; teq (var_ty "b") bool_ty] in
  test_principle_type ty cs (Some (Forall (VarSet.empty, (pair_ty int_ty bool_ty))))

(* Complex chained substitutions test *)
let test7 () =
  let ty = var_ty "d" in
  let cs = [
    teq (var_ty "d") (var_ty "c");
    teq (var_ty "c") (fun_ty (var_ty "a") (var_ty "b"));
    teq (var_ty "a") int_ty;
    teq (var_ty "b") bool_ty
  ] in
  test_principle_type ty cs (Some (Forall (VarSet.empty, (fun_ty int_ty bool_ty))))

(* Multiple variable substitutions in nested types *)
let test8 () =
  let ty = var_ty "x" in
  let cs = [
    teq (var_ty "x") (list_ty (fun_ty (var_ty "a") (var_ty "b")));
    teq (var_ty "a") int_ty;
    teq (var_ty "b") (var_ty "c");
    teq (var_ty "c") bool_ty
  ] in
  test_principle_type ty cs (Some (Forall (VarSet.empty, (list_ty (fun_ty int_ty bool_ty)))))

(* Higher-order function types with substitutions *)
let test9 () =
  let ty = var_ty "f" in
  let cs = [
    teq (var_ty "f") (fun_ty (fun_ty (var_ty "a") (var_ty "b")) (var_ty "c"));
    teq (var_ty "a") int_ty;
    teq (var_ty "b") bool_ty;
    teq (var_ty "c") (var_ty "d");
    teq (var_ty "d") (list_ty (var_ty "a"))
  ] in
  test_principle_type ty cs (Some (Forall (VarSet.empty, 
    (fun_ty (fun_ty int_ty bool_ty) (list_ty int_ty)))))

(* Test with reversed order of constraints *)
let test10 () =
  let ty = var_ty "c" in
  let cs = [
    teq (var_ty "b") bool_ty;
    teq (var_ty "a") int_ty;
    teq (var_ty "c") (fun_ty (var_ty "a") (var_ty "b"))
  ] in
  test_principle_type ty cs (Some (Forall (VarSet.empty, (fun_ty int_ty bool_ty))))

(* Complex nested substitutions involving data structures *)
let test11 () =
  let ty = var_ty "r" in
  let cs = [
    teq (var_ty "r") (pair_ty (var_ty "a") (var_ty "b"));
    teq (var_ty "a") (list_ty (var_ty "c"));
    teq (var_ty "b") (option_ty (var_ty "d"));
    teq (var_ty "c") (var_ty "e");
    teq (var_ty "d") (var_ty "e");
    teq (var_ty "e") int_ty
  ] in
  test_principle_type ty cs (Some (Forall (VarSet.empty, 
    (pair_ty (list_ty int_ty) (option_ty int_ty)))))

(* Test type_of on expressions *)
let test_type_of_exprs () =
  Printf.printf "\nRunning type_of (expression) tests...\n\n";

  (* id = fun x -> x *)
  test_type_of "fun x -> x"
    (Fun ("x", None, Var "x"))
    (TFun (TVar "a", TVar "a"));

  (* const = fun x -> fun y -> x *)
  test_type_of "fun x -> fun y -> x"
    (Fun ("x", None, Fun ("y", None, Var "x")))
    (TFun (TVar "a", TFun (TVar "b", TVar "a")));

  (* add1 = fun x -> x + 1 *)
  test_type_of "fun x -> x + 1"
    (Fun ("x", None, Bop (Add, Var "x", Int 1)))
    (TFun (TInt, TInt));

  (* let x = 1 in x + 2 *)
  test_type_of "let x = 1 in x + 2"
    (Let { is_rec = false; name = "x"; binding = Int 1; body = Bop (Add, Var "x", Int 2) })
    TInt;

  (* match x with None -> 0 | Some y -> y + 1 *)
  test_type_of "match x with | None -> 0 | Some y -> y + 1"
    (OptMatch { 
      matched = Var "x"; 
      some_name = "y"; 
      some_case = Bop (Add, Var "y", Int 1); 
      none_case = Int 0 
    })
    TInt;

  (* if x then 1 else 2 *)
  test_type_of "if x then 1 else 2"
    (If (Var "x", Int 1, Int 2))
    TInt;

  (* List operations *)
  test_type_of "[]"
    Nil
    (TList (TVar "a"));

  test_type_of "x :: xs"
    (Bop (Cons, Var "x", Var "xs"))
    (TList (TVar "a"));

  test_type_of "match xs with | [] -> 0 | h :: t -> h"
    (ListMatch {
      matched = Var "xs";
      hd_name = "h";
      tl_name = "t";
      cons_case = Var "h";
      nil_case = Int 0
    })
    TInt;

  (* Pair operations *)
  test_type_of "(1, true)"
    (Bop (Comma, Int 1, Bool true))
    (TPair (TInt, TBool));

  test_type_of "match p with | (x, y) -> x + y"
    (PairMatch {
      matched = Var "p";
      fst_name = "x";
      snd_name = "y";
      case = Bop (Add, Var "x", Var "y")
    })
    TInt;

  (* Function with type annotation *)
  test_type_of "fun (x : int) -> x + 1"
    (Fun ("x", Some TInt, Bop (Add, Var "x", Int 1)))
    (TFun (TInt, TInt));

  (* Expression with type annotation *)
  test_type_of "(x + 1 : int)"
    (Annot (Bop (Add, Var "x", Int 1), TInt))
    TInt;

  (* Recursive function *)
  test_type_of "let rec f = fun x -> if x = 0 then 1 else x * f (x - 1) in f 5"
    (Let {
      is_rec = true;
      name = "f";
      binding = Fun ("x", None, 
                    If (Bop (Eq, Var "x", Int 0),
                        Int 1,
                        Bop (Mul, Var "x", 
                             App (Var "f", Bop (Sub, Var "x", Int 1)))));
      body = App (Var "f", Int 5)
    })
    TInt;

  (* Polymorphic functions *)
  let id_env = [("id", Forall (VarSet.singleton "a", TFun (TVar "a", TVar "a")))] in
  test_type_of ~ctx:id_env "id 5"
    (App (Var "id", Int 5))
    TInt;

  test_type_of ~ctx:id_env "id true"
    (App (Var "id", Bool true))
    TBool;

  (* Fold function test *)
  let fold_env = [
    ("fold_left", 
     Forall (
       VarSet.of_list ["a"; "b"],
       TFun (
         TFun (TVar "a", TFun (TVar "b", TVar "a")),
         TFun (TVar "a", TFun (TList (TVar "b"), TVar "a"))
       )
     ))
  ] in
  
  test_type_of ~ctx:fold_env "fold_left (fun acc x -> acc + x) 0 [1;2;3]"
    (App (
      App (
        App (
          Var "fold_left",
          Fun ("acc", None, Fun ("x", None, Bop (Add, Var "acc", Var "x")))
        ),
        Int 0
      ),
      (* This represents [1;2;3] *)
      Bop (Cons, Int 1, Bop (Cons, Int 2, Bop (Cons, Int 3, Nil)))
    ))
    TInt;

  (* Newton's method test (simplified) *)
  test_type_of "let f = fun x -> x *. x -. 2.0 in f 1.4"
    (Let {
      is_rec = false;
      name = "f";
      binding = Fun ("x", None, 
                    Bop (SubF, 
                         Bop (MulF, Var "x", Var "x"),
                         Float 2.0));
      body = App (Var "f", Float 1.4)
    })
    TFloat;

  (* Assertions test *)
  test_type_of "assert (x > 0)"
    (Assert (Bop (Gt, Var "x", Int 0)))
    TUnit;

  (* assert false special case *)
  test_type_of "assert false"
    (Assert (Bool false))
    (TVar "a")

(* Main function to run all tests *)
let main () =
  (* Reset counters *)
  passed := 0;
  failed := 0;

  Printf.printf "Running principle_type tests...\n\n";
  test1 ();
  test2 ();
  test3 ();
  test4 ();
  test5 ();
  test6 ();
  test7 ();
  test8 ();
  test9 ();
  test10 ();
  test11 ();
  
  test_type_of_exprs ();
  
  (* Print summary *)
  Printf.printf "\n--------- TEST SUMMARY ---------\n";
  Printf.printf "PASSED: %d\n" !passed;
  Printf.printf "FAILED: %d\n" !failed;
  Printf.printf "TOTAL: %d\n" (!passed + !failed);
  Printf.printf "Success rate: %.2f%%\n" 
    (float_of_int !passed *. 100.0 /. float_of_int (!passed + !failed))

let () = main ()