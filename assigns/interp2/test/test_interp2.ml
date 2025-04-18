open Interp2
open OUnit2

(* Helper function to parse a string into a prog *)
let parse_prog s =
  match parse s with
  | Some p -> p
  | None -> failwith "Parse error"

(* Desugar Tests *)
let desugar_tests =
  "desugar test suite" >:::
    [
      (* Test 1: Empty program *)
      "empty program" >:: (fun _ ->
        let prog = parse_prog "" in
        let desugared = desugar prog in
        assert_equal Unit desugared);

      (* Test 2: Simple let statement *)
      "simple let" >:: (fun _ ->
        let prog = parse_prog "let x : int = 5" in
        let desugared = desugar prog in
        match desugared with
        | Let {is_rec = false; name = "x"; binding = Num 5; body = Var "x"; _} -> ()
        | _ -> assert_failure "Incorrect desugaring of simple let");

      (* Test 3: Let with function arguments *)
      "let with function args" >:: (fun _ ->
        let prog = parse_prog "let f (x : int) : int = x + 1" in
        let desugared = desugar prog in
        match desugared with
        | Let {binding = Fun (arg, _, _); _} -> 
            assert_equal "x" arg ~msg:"Argument name should be preserved"
        | _ -> assert_failure "Incorrect desugaring of function");

      (* Test 4: Multiple top-level statements *)
      "multiple top-level statements" >:: (fun _ ->
        let prog = parse_prog "let x : int = 5\nlet y : int = x + 1" in
        let desugared = desugar prog in
        match desugared with
        | Let {name = "x"; body = Let {name = "y"; body = Var "y"; _}; _} -> ()
        | _ -> assert_failure "Incorrect nesting of let expressions");

      (* Test 5: Multi-argument function *)
      "multi-argument function" >:: (fun _ ->
        let prog = parse_prog "let f (x : int) (y : int) : int = x + y" in
        let desugared = desugar prog in
        match desugared with
        | Let {binding = Fun (x, _, Fun (y, _, _)); _} ->
            assert_equal "x" x ~msg:"First argument name should be preserved";
            assert_equal "y" y ~msg:"Second argument name should be preserved"
        | _ -> assert_failure "Incorrect currying of multi-argument function");

      (* Test 6: Let-rec statement *)
      "let rec statement" >:: (fun _ ->
        let prog = parse_prog "let rec fact (n : int) : int = if n <= 1 then 1 else n * fact (n - 1)" in
        let desugared = desugar prog in
        match desugared with
        | Let {is_rec = true; name = "fact"; _} -> ()
        | _ -> assert_failure "Recursive flag not preserved");
    ]

(* Type Checker Tests *)
let type_of_tests =
  "type_of test suite" >:::
    [
      "integer literal" >:: (fun _ ->
        let expr = Num 5 in
        let expected = Ok IntTy in
        let actual = type_of expr in
        assert_equal expected actual);

      "boolean literal" >:: (fun _ ->
        let expr = Bool true in
        let expected = Ok BoolTy in
        let actual = type_of expr in
        assert_equal expected actual);

      "unit literal" >:: (fun _ ->
        let expr = Unit in
        let expected = Ok UnitTy in
        let actual = type_of expr in
        assert_equal expected actual);

      "function type" >:: (fun _ ->
        let expr = Fun ("x", IntTy, Var "x") in
        let expected = Ok (FunTy (IntTy, IntTy)) in
        let actual = type_of expr in
        assert_equal expected actual);

      "unknown variable" >:: (fun _ ->
        let expr = Var "x" in
        let expected = Error (UnknownVar "x") in
        let actual = type_of expr in
        assert_equal expected actual);

      "add operation" >:: (fun _ ->
        let expr = Bop (Add, Num 1, Num 2) in
        let expected = Ok IntTy in
        let actual = type_of expr in
        assert_equal expected actual);

      "add with wrong type" >:: (fun _ ->
        let expr = Bop (Add, Num 1, Bool true) in
        let expected = Error (OpTyErrR (Add, IntTy, BoolTy)) in
        let actual = type_of expr in
        assert_equal expected actual);

      "comparison" >:: (fun _ ->
        let expr = Bop (Lt, Num 1, Num 2) in
        let expected = Ok BoolTy in
        let actual = type_of expr in
        assert_equal expected actual);

      "if expression" >:: (fun _ ->
        let expr = If (Bool true, Num 1, Num 2) in
        let expected = Ok IntTy in
        let actual = type_of expr in
        assert_equal expected actual);

      "if with wrong condition" >:: (fun _ ->
        let expr = If (Num 1, Num 2, Num 3) in
        let expected = Error (IfCondTyErr IntTy) in
        let actual = type_of expr in
        assert_equal expected actual);

      "if with mismatched branches" >:: (fun _ ->
        let expr = If (Bool true, Num 1, Bool false) in
        let expected = Error (IfTyErr (IntTy, BoolTy)) in
        let actual = type_of expr in
        assert_equal expected actual);

      "function application" >:: (fun _ ->
        let expr = App (Fun ("x", IntTy, Var "x"), Num 5) in
        let expected = Ok IntTy in
        let actual = type_of expr in
        assert_equal expected actual);

      "function application with wrong arg type" >:: (fun _ ->
        let expr = App (Fun ("x", IntTy, Var "x"), Bool true) in
        let expected = Error (FunArgTyErr (IntTy, BoolTy)) in
        let actual = type_of expr in
        assert_equal expected actual);

      "non-function application" >:: (fun _ ->
        let expr = App (Num 3, Num 4) in
        let expected = Error (FunAppTyErr IntTy) in
        let actual = type_of expr in
        assert_equal expected actual);

      "let expression" >:: (fun _ ->
        let expr = Let {is_rec = false; name = "x"; ty = IntTy; binding = Num 5; body = Var "x"} in
        let expected = Ok IntTy in
        let actual = type_of expr in
        assert_equal expected actual);

      "let with type mismatch" >:: (fun _ ->
        let expr = Let {is_rec = false; name = "x"; ty = BoolTy; binding = Num 5; body = Var "x"} in
        let expected = Error (LetTyErr (BoolTy, IntTy)) in
        let actual = type_of expr in
        assert_equal expected actual);

      "recursive function" >:: (fun _ ->
        let expr = Let {
          is_rec = true; 
          name = "fact"; 
          ty = FunTy (IntTy, IntTy);
          binding = Fun ("n", IntTy, 
                         If (Bop (Lte, Var "n", Num 1), 
                             Num 1, 
                             Bop (Mul, Var "n", App (Var "fact", Bop (Sub, Var "n", Num 1))))
                        );
          body = Var "fact"
        } in
        let expected = Ok (FunTy (IntTy, IntTy)) in
        let actual = type_of expr in
        assert_equal expected actual);

      "assert expression" >:: (fun _ ->
        let expr = Assert (Bool true) in
        let expected = Ok UnitTy in
        let actual = type_of expr in
        assert_equal expected actual);

      "assert with wrong type" >:: (fun _ ->
        let expr = Assert (Num 5) in
        let expected = Error (AssertTyErr IntTy) in
        let actual = type_of expr in
        assert_equal expected actual);
    ]

(* Evaluation Tests *)
let eval_tests =
  "eval test suite" >:::
    [
      "integer literal" >:: (fun _ ->
        let expr = Num 5 in
        let expected = VNum 5 in
        let actual = eval expr in
        assert_equal expected actual);

      "boolean literal" >:: (fun _ ->
        let expr = Bool true in
        let expected = VBool true in
        let actual = eval expr in
        assert_equal expected actual);

      "unit literal" >:: (fun _ ->
        let expr = Unit in
        let expected = VUnit in
        let actual = eval expr in
        assert_equal expected actual);

      "add operation" >:: (fun _ ->
        let expr = Bop (Add, Num 1, Num 2) in
        let expected = VNum 3 in
        let actual = eval expr in
        assert_equal expected actual);

      "sub operation" >:: (fun _ ->
        let expr = Bop (Sub, Num 5, Num 2) in
        let expected = VNum 3 in
        let actual = eval expr in
        assert_equal expected actual);

      "mul operation" >:: (fun _ ->
        let expr = Bop (Mul, Num 3, Num 2) in
        let expected = VNum 6 in
        let actual = eval expr in
        assert_equal expected actual);

      "div operation" >:: (fun _ ->
        let expr = Bop (Div, Num 6, Num 2) in
        let expected = VNum 3 in
        let actual = eval expr in
        assert_equal expected actual);

      "mod operation" >:: (fun _ ->
        let expr = Bop (Mod, Num 7, Num 3) in
        let expected = VNum 1 in
        let actual = eval expr in
        assert_equal expected actual);

      "lt operation (true)" >:: (fun _ ->
        let expr = Bop (Lt, Num 1, Num 2) in
        let expected = VBool true in
        let actual = eval expr in
        assert_equal expected actual);

      "lt operation (false)" >:: (fun _ ->
        let expr = Bop (Lt, Num 2, Num 1) in
        let expected = VBool false in
        let actual = eval expr in
        assert_equal expected actual);

      "and operation (true)" >:: (fun _ ->
        let expr = Bop (And, Bool true, Bool true) in
        let expected = VBool true in
        let actual = eval expr in
        assert_equal expected actual);

      "and operation (false)" >:: (fun _ ->
        let expr = Bop (And, Bool true, Bool false) in
        let expected = VBool false in
        let actual = eval expr in
        assert_equal expected actual);

      "or operation (true)" >:: (fun _ ->
        let expr = Bop (Or, Bool false, Bool true) in
        let expected = VBool true in
        let actual = eval expr in
        assert_equal expected actual);

      "or operation (false)" >:: (fun _ ->
        let expr = Bop (Or, Bool false, Bool false) in
        let expected = VBool false in
        let actual = eval expr in
        assert_equal expected actual);

      "if expression (true branch)" >:: (fun _ ->
        let expr = If (Bool true, Num 1, Num 2) in
        let expected = VNum 1 in
        let actual = eval expr in
        assert_equal expected actual);

      "if expression (false branch)" >:: (fun _ ->
        let expr = If (Bool false, Num 1, Num 2) in
        let expected = VNum 2 in
        let actual = eval expr in
        assert_equal expected actual);

      "let expression" >:: (fun _ ->
        let expr = Let {is_rec = false; name = "x"; ty = IntTy; binding = Num 5; body = Var "x"} in
        let expected = VNum 5 in
        let actual = eval expr in
        assert_equal expected actual);

      "function application" >:: (fun _ ->
        let expr = App (Fun ("x", IntTy, Bop (Add, Var "x", Num 1)), Num 5) in
        let expected = VNum 6 in
        let actual = eval expr in
        assert_equal expected actual);

      "recursive function (factorial)" >:: (fun _ ->
        let expr = Let {
          is_rec = true; 
          name = "fact"; 
          ty = FunTy (IntTy, IntTy);
          binding = Fun ("n", IntTy, 
                         If (Bop (Lte, Var "n", Num 1), 
                             Num 1, 
                             Bop (Mul, Var "n", App (Var "fact", Bop (Sub, Var "n", Num 1))))
                        );
          body = App (Var "fact", Num 5)
        } in
        let expected = VNum 120 in
        let actual = eval expr in
        assert_equal expected actual);

      "assert true" >:: (fun _ ->
        let expr = Assert (Bool true) in
        let expected = VUnit in
        let actual = eval expr in
        assert_equal expected actual);
    ]

(* Integration Tests for Interp Function *)
let interp_tests =
  "interp test suite" >:::
    [
      "basic evaluation" >:: (fun _ ->
        let result = interp "let x : int = 5" in
        assert_equal (Ok (VNum 5)) result);

      "function application" >:: (fun _ ->
        let result = interp "let f (x : int) : int = x + 1\nlet _ : int = f 5" in
        assert_equal (Ok (VNum 6)) result);

      "recursive function" >:: (fun _ ->
        let result = interp "let rec fact (n : int) : int = if n <= 1 then 1 else n * fact (n - 1)\nlet _ : int = fact 5" in
        assert_equal (Ok (VNum 120)) result);

      "parse error" >:: (fun _ ->
        let result = interp "let 5 + + 3" in
        assert_equal (Error ParseErr) result);

      "type error" >:: (fun _ ->
        let result = interp "let _ : int = true" in
        match result with
        | Error (LetTyErr _) -> ()
        | _ -> assert_failure "Expected type error");

      "sum of squares" >:: (fun _ ->
        let result = interp
          "let sum_of_squares (x : int) (y : int) : int =
             let x_squared : int = x * x in
             let y_squared : int = y * y in
             x_squared + y_squared
           let _ : unit = assert (sum_of_squares 3 (-5) = 34)"
        in
        assert_equal (Ok VUnit) result);

      "operators" >:: (fun _ ->
        let result = interp
          "let _ : unit = 
             let a : int = 10 in
             let b : int = 5 in
             assert (a + b = 15);
             assert (a - b = 5);
             assert (a * b = 50);
             assert (a / b = 2);
             assert (a mod b = 0);
             assert (a < 20);
             assert (a <= 10);
             assert (b > 2);
             assert (b >= 5);
             assert (a = 10);
             assert (a <> 11);
             assert (true && true);
             assert (true || false)"
        in
        assert_equal (Ok VUnit) result);

      "fibonacci" >:: (fun _ ->
        let result = interp
          "let rec fibonacci (n : int) : int =
             if n <= 1 then n
             else fibonacci (n - 1) + fibonacci (n - 2)
           let _ : unit = assert (fibonacci 0 = 0)
           let _ : unit = assert (fibonacci 1 = 1)
           let _ : unit = assert (fibonacci 5 = 5)
           let _ : unit = assert (fibonacci 10 = 55)"
        in
        assert_equal (Ok VUnit) result);

      "division by zero" >:: (fun _ ->
        let result = interp "let _ : int = 1 / 0" in
        match result with
        | Error _ -> ()  (* Just check it's an error, not what specific error *)
        | Ok _ -> assert_failure "Expected error from division by zero");

      "assertion failure" >:: (fun _ ->
        let result = interp "let _ : unit = assert (false)" in
        match result with
        | Error _ -> ()  (* Just check it's an error, not what specific error *)
        | Ok _ -> assert_failure "Expected error from assert failure");

      "unbound variable" >:: (fun _ ->
        let result = interp "let _ : int = x + 1" in
        assert_equal (Error (UnknownVar "x")) result);
    ]

(* Complete Test Suite *)
let tests =
  "complete interp2 test suite" >:::
    [
      desugar_tests;
      type_of_tests;
      eval_tests;
      interp_tests;
    ]

let _ = run_test_tt_main tests