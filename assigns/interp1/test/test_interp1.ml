open Interp1
open OUnit2

(* Helper function to convert result to string for printing in test output *)
let string_of_result ok_printer err_printer = function
  | Ok v -> "Ok (" ^ ok_printer v ^ ")"
  | Error e -> "Error (" ^ err_printer e ^ ")"

(* Printer for evaluation results *)
let string_of_eval_result = string_of_result string_of_value string_of_error

let parse_tests =
  "testing parse" >:::
    [
      "basic arithmetic expr" >:: (fun _ ->
        let expected = Some (Bop (Mul, Bop (Add, Num 1, Num 2), Num 3)) in
        let actual = parse "(1 + 2) * 3" in
        assert_equal expected actual);
        
      "parse simple number" >:: (fun _ ->
        let expected = Some (Num 42) in
        let actual = parse "42" in
        assert_equal expected actual);
        
      "parse negative number" >:: (fun _ ->
        let expected = Some (Num (-7)) in
        let actual = parse "-7" in
        assert_equal expected actual);
        
      "parse boolean value" >:: (fun _ ->
        let expected = Some True in
        let actual = parse "true" in
        assert_equal expected actual);
        
      "parse unit value" >:: (fun _ ->
        let expected = Some Unit in
        let actual = parse "()" in
        assert_equal expected actual);
        
      "parse variable" >:: (fun _ ->
        let expected = Some (Var "x") in
        let actual = parse "x" in
        assert_equal expected actual);
        
      "parse complex variable name" >:: (fun _ ->
        let expected = Some (Var "my_var_123") in
        let actual = parse "my_var_123" in
        assert_equal expected actual);
        
      "parse addition" >:: (fun _ ->
        let expected = Some (Bop (Add, Num 5, Num 3)) in
        let actual = parse "5 + 3" in
        assert_equal expected actual);
        
      "parse subtraction" >:: (fun _ ->
        let expected = Some (Bop (Sub, Num 10, Num 4)) in
        let actual = parse "10 - 4" in
        assert_equal expected actual);
        
      "parse multiplication" >:: (fun _ ->
        let expected = Some (Bop (Mul, Num 6, Num 7)) in
        let actual = parse "6 * 7" in
        assert_equal expected actual);
        
      "parse division" >:: (fun _ ->
        let expected = Some (Bop (Div, Num 20, Num 5)) in
        let actual = parse "20 / 5" in
        assert_equal expected actual);
        
      "parse modulo" >:: (fun _ ->
        let expected = Some (Bop (Mod, Num 17, Num 5)) in
        let actual = parse "17 mod 5" in
        assert_equal expected actual);
        
      "parse comparison operators" >:: (fun _ ->
        let expected = Some (Bop (Lt, Num 3, Num 5)) in
        let actual = parse "3 < 5" in
        assert_equal expected actual);
        
      "parse logical operators" >:: (fun _ ->
        let expected = Some (Bop (And, True, False)) in
        let actual = parse "true && false" in
        assert_equal expected actual);
        
      "parse if expression" >:: (fun _ ->
        let expected = Some (If (True, Num 1, Num 0)) in
        let actual = parse "if true then 1 else 0" in
        assert_equal expected actual);
        
      "parse let expression" >:: (fun _ ->
        let expected = Some (Let ("x", Num 5, Bop (Add, Var "x", Num 3))) in
        let actual = parse "let x = 5 in x + 3" in
        assert_equal expected actual);
        
      "parse function definition" >:: (fun _ ->
        let expected = Some (Fun ("x", Bop (Add, Var "x", Num 1))) in
        let actual = parse "fun x -> x + 1" in
        assert_equal expected actual);
        
      "parse function application" >:: (fun _ ->
        let expected = Some (App (Var "f", Num 3)) in
        let actual = parse "f 3" in
        assert_equal expected actual);
        
      "parse nested function application" >:: (fun _ ->
        let expected = Some (App (App (Var "f", Num 1), Num 2)) in
        let actual = parse "f 1 2" in
        assert_equal expected actual);
        
      "parse complex expression with precedence" >:: (fun _ ->
        let expected = Some (Bop (Add, Num 1, Bop (Mul, Num 2, Num 3))) in
        let actual = parse "1 + 2 * 3" in
        assert_equal expected actual);
        
      "parse invalid input" >:: (fun _ ->
        let expected = None in
        let actual = parse "invalid syntax @#$" in
        assert_equal expected actual);
    ]

let subst_tests =
  "testing subst" >:::
    [
      "single variable" >:: (fun _ ->
        let expected = Bop (Add, Var "x", If (Unit, Unit, Unit)) in
        let actual = subst VUnit "y" (Bop (Add, Var "x", If(Var "y", Var "y", Var "y"))) in
        assert_equal expected actual);
        
      "no substitution needed" >:: (fun _ ->
        let expr = Bop (Add, Num 1, Num 2) in
        let expected = expr in
        let actual = subst (VNum 5) "x" expr in
        assert_equal expected actual);
        
      "substitute number value" >:: (fun _ ->
        let expected = Bop (Add, Num 5, Num 2) in
        let actual = subst (VNum 5) "x" (Bop (Add, Var "x", Num 2)) in
        assert_equal expected actual);
        
      "substitute boolean value" >:: (fun _ ->
        let expected = If (True, Num 1, Num 2) in
        let actual = subst (VBool true) "b" (If (Var "b", Num 1, Num 2)) in
        assert_equal expected actual);
        
      "substitute function value" >:: (fun _ ->
        let fun_value = VFun ("y", Bop (Add, Var "y", Num 1)) in
        let expected = App (Fun ("y", Bop (Add, Var "y", Num 1)), Num 3) in
        let actual = subst fun_value "f" (App (Var "f", Num 3)) in
        assert_equal expected actual);
        
      "substitution with shadowing" >:: (fun _ ->
        let expected = Let ("x", Num 10, Var "x") in
        let actual = subst (VNum 5) "x" (Let ("x", Num 10, Var "x")) in
        assert_equal expected actual);
        
      "substitution in nested expressions" >:: (fun _ ->
        let expected = If (True, Bop (Add, Num 5, Num 3), Num 0) in
        let actual = subst (VBool true) "c" (If (Var "c", Bop (Add, Num 5, Num 3), Num 0)) in
        assert_equal expected actual);
        
      "substitution in function body" >:: (fun _ ->
        let expected = Fun ("y", Bop (Add, Var "y", Num 5)) in
        let actual = subst (VNum 5) "x" (Fun ("y", Bop (Add, Var "y", Var "x"))) in
        assert_equal expected actual);
        
      "no substitution with shadowed variable" >:: (fun _ ->
        let expected = Fun ("x", Bop (Add, Var "x", Num 1)) in
        let actual = subst (VNum 5) "x" (Fun ("x", Bop (Add, Var "x", Num 1))) in
        assert_equal expected actual);
    ]

let eval_tests =
  "testing eval" >:::
    [
      "application" >:: (fun _ ->
        let expected = Ok (VNum 4) in
        let actual = eval (App (Fun ("x", Bop (Add, Var "x", Num 1)), Num 3)) in
        assert_equal expected actual ~printer:string_of_eval_result);
        
      "evaluate simple number" >:: (fun _ ->
        let expected = Ok (VNum 42) in
        let actual = eval (Num 42) in
        assert_equal expected actual ~printer:string_of_eval_result);
        
      "evaluate boolean" >:: (fun _ ->
        let expected = Ok (VBool true) in
        let actual = eval True in
        assert_equal expected actual ~printer:string_of_eval_result);
        
      "evaluate unit" >:: (fun _ ->
        let expected = Ok VUnit in
        let actual = eval Unit in
        assert_equal expected actual ~printer:string_of_eval_result);
        
      "evaluate addition" >:: (fun _ ->
        let expected = Ok (VNum 8) in
        let actual = eval (Bop (Add, Num 5, Num 3)) in
        assert_equal expected actual ~printer:string_of_eval_result);
        
      "evaluate subtraction" >:: (fun _ ->
        let expected = Ok (VNum 2) in
        let actual = eval (Bop (Sub, Num 5, Num 3)) in
        assert_equal expected actual ~printer:string_of_eval_result);
        
      "evaluate multiplication" >:: (fun _ ->
        let expected = Ok (VNum 15) in
        let actual = eval (Bop (Mul, Num 5, Num 3)) in
        assert_equal expected actual ~printer:string_of_eval_result);
        
      "evaluate division" >:: (fun _ ->
        let expected = Ok (VNum 2) in
        let actual = eval (Bop (Div, Num 5, Num 2)) in
        assert_equal expected actual ~printer:string_of_eval_result);
        
      "evaluate division by zero" >:: (fun _ ->
        let expected = Error DivByZero in
        let actual = eval (Bop (Div, Num 5, Num 0)) in
        assert_equal expected actual ~printer:string_of_eval_result);
        
      "evaluate modulo" >:: (fun _ ->
        let expected = Ok (VNum 2) in
        let actual = eval (Bop (Mod, Num 5, Num 3)) in
        assert_equal expected actual ~printer:string_of_eval_result);
        
      "evaluate less than (true)" >:: (fun _ ->
        let expected = Ok (VBool true) in
        let actual = eval (Bop (Lt, Num 3, Num 5)) in
        assert_equal expected actual ~printer:string_of_eval_result);
        
      "evaluate less than (false)" >:: (fun _ ->
        let expected = Ok (VBool false) in
        let actual = eval (Bop (Lt, Num 5, Num 3)) in
        assert_equal expected actual ~printer:string_of_eval_result);
        
      "evaluate and (true)" >:: (fun _ ->
        let expected = Ok (VBool true) in
        let actual = eval (Bop (And, True, True)) in
        assert_equal expected actual ~printer:string_of_eval_result);
        
      "evaluate and (false)" >:: (fun _ ->
        let expected = Ok (VBool false) in
        let actual = eval (Bop (And, True, False)) in
        assert_equal expected actual ~printer:string_of_eval_result);
        
      "evaluate or (true)" >:: (fun _ ->
        let expected = Ok (VBool true) in
        let actual = eval (Bop (Or, False, True)) in
        assert_equal expected actual ~printer:string_of_eval_result);
        
      "evaluate or (false)" >:: (fun _ ->
        let expected = Ok (VBool false) in
        let actual = eval (Bop (Or, False, False)) in
        assert_equal expected actual ~printer:string_of_eval_result);
        
      "evaluate if (true branch)" >:: (fun _ ->
        let expected = Ok (VNum 1) in
        let actual = eval (If (True, Num 1, Num 0)) in
        assert_equal expected actual ~printer:string_of_eval_result);
        
      "evaluate if (false branch)" >:: (fun _ ->
        let expected = Ok (VNum 0) in
        let actual = eval (If (False, Num 1, Num 0)) in
        assert_equal expected actual ~printer:string_of_eval_result);
        
      "evaluate if (invalid condition)" >:: (fun _ ->
        let expected = Error InvalidIfCond in
        let actual = eval (If (Num 5, Num 1, Num 0)) in
        assert_equal expected actual ~printer:string_of_eval_result);
        
      "evaluate let" >:: (fun _ ->
        let expected = Ok (VNum 8) in
        let actual = eval (Let ("x", Num 5, Bop (Add, Var "x", Num 3))) in
        assert_equal expected actual ~printer:string_of_eval_result);
        
      "evaluate function" >:: (fun _ ->
        let expected = Ok (VFun ("x", Bop (Add, Var "x", Num 1))) in
        let actual = eval (Fun ("x", Bop (Add, Var "x", Num 1))) in
        assert_equal expected actual ~printer:string_of_eval_result);
        
      "evaluate invalid application" >:: (fun _ ->
        let expected = Error InvalidApp in
        let actual = eval (App (Num 5, Num 3)) in
        assert_equal expected actual ~printer:string_of_eval_result);
        
      "evaluate invalid arguments" >:: (fun _ ->
        let expected = Error (InvalidArgs Add) in
        let actual = eval (Bop (Add, True, Num 3)) in
        assert_equal expected actual ~printer:string_of_eval_result);
        
      "evaluate unknown variable" >:: (fun _ ->
        let expected = Error (UnknownVar "x") in
        let actual = eval (Var "x") in
        assert_equal expected actual ~printer:string_of_eval_result);
        
      "evaluate short-circuit and (false)" >:: (fun _ ->
        let expected = Ok (VBool false) in
        let actual = eval (Bop (And, False, Var "x")) in
        assert_equal expected actual ~printer:string_of_eval_result);
        
      "evaluate short-circuit or (true)" >:: (fun _ ->
        let expected = Ok (VBool true) in
        let actual = eval (Bop (Or, True, Var "x")) in
        assert_equal expected actual ~printer:string_of_eval_result);
    ]

let interp_tests =
  "interp tests" >:::
    [
      "variable" >:: (fun _ ->
        let expected = Ok (VBool true) in
        let actual = interp "let x = true || false in x && true" in
        assert_equal expected actual ~printer:string_of_eval_result);
        
      "interpret simple number" >:: (fun _ ->
        let expected = Ok (VNum 42) in
        let actual = interp "42" in
        assert_equal expected actual ~printer:string_of_eval_result);
        
      "interpret arithmetic" >:: (fun _ ->
        let expected = Ok (VNum 7) in
        let actual = interp "1 + 2 * 3" in
        assert_equal expected actual ~printer:string_of_eval_result);
        
      "interpret function application" >:: (fun _ ->
        let expected = Ok (VNum 4) in
        let actual = interp "(fun x -> x + 1) 3" in
        assert_equal expected actual ~printer:string_of_eval_result);
        
      "interpret nested let" >:: (fun _ ->
        let expected = Ok (VNum 15) in
        let actual = interp "let x = 5 in let y = 3 in x * y" in
        assert_equal expected actual ~printer:string_of_eval_result);
        
      "interpret conditional" >:: (fun _ ->
        let expected = Ok (VNum 10) in
        let actual = interp "if 5 > 3 then 10 else 20" in
        assert_equal expected actual ~printer:string_of_eval_result);
        
      "interpret division by zero" >:: (fun _ ->
        let expected = Error DivByZero in
        let actual = interp "5 / 0" in
        assert_equal expected actual ~printer:string_of_eval_result);
        
      "interpret invalid if condition" >:: (fun _ ->
        let expected = Error InvalidIfCond in
        let actual = interp "if 42 then true else false" in
        assert_equal expected actual ~printer:string_of_eval_result);
        
      "interpret invalid arguments" >:: (fun _ ->
        let expected = Error (InvalidArgs Add) in
        let actual = interp "1 + true" in
        assert_equal expected actual ~printer:string_of_eval_result);
        
      "interpret invalid application" >:: (fun _ ->
        let expected = Error InvalidApp in
        let actual = interp "42 10" in
        assert_equal expected actual ~printer:string_of_eval_result);
        
      "interpret unknown variable" >:: (fun _ ->
        let expected = Error (UnknownVar "x") in
        let actual = interp "x" in
        assert_equal expected actual ~printer:string_of_eval_result);
        
      "interpret parse failure" >:: (fun _ ->
        let expected = Error ParseFail in
        let actual = interp "@#$%" in
        assert_equal expected actual ~printer:string_of_eval_result);
        
      "interpret sum of squares example" >:: (fun _ ->
        let expected = Ok (VNum 34) in
        let program = "let sum_of_squares = fun x -> fun y ->
                        let x_squared = x * x in
                        let y_squared = y * y in
                        x_squared + y_squared
                      in sum_of_squares 3 (-5)" in
        let actual = interp program in
        assert_equal expected actual ~printer:string_of_eval_result);
    ]

let tests =
  "interp1 test suite" >:::
    [
      parse_tests;
      subst_tests;
      eval_tests;
      interp_tests;
    ]

let _ = run_test_tt_main tests