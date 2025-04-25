(* test_interp3.ml - Testing file for interp3 *)
open Interp3

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
  if result_str = expected_str then
    Printf.printf "PASS: principle_type %s %s = %s\n" 
      (string_of_ty ty) 
      (string_of_constrs cs) 
      result_str
  else
    Printf.printf "FAIL: principle_type %s %s\n  Expected: %s\n  Got: %s\n" 
      (string_of_ty ty) 
      (string_of_constrs cs) 
      expected_str 
      result_str

(* Helper functions to create types and constraints *)
let int_ty = TInt
let bool_ty = TBool
(* Removed unused float_ty and unit_ty *)
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

(* Run all tests *)
let run_tests () =
  Printf.printf "Running principle_type tests...\n\n";
  test1 ();
  test2 ();
  test3 ();
  test4 ();
  test5 ();
  test6 ();
  Printf.printf "\nTests completed.\n"

let () = run_tests ()