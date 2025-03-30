open Interp1
open OUnit2

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
      (* TODO: write more tests *)
    ]

let eval_tests =
  "testing eval" >:::
    [
      "application" >:: (fun _ ->
        let expected = Ok (VNum 4) in
        let actual = eval (App (Fun ("x", Bop (Add, Var "x", Num 1)), Num 3)) in
        assert_equal expected actual);
      (* TODO: write more tests *)
    ]

let interp_tests =
  "interp tests" >:::
    [
      "variable" >:: (fun _ ->
        let expected = Ok (VBool true) in
        let actual = interp "let x = true || false in x && true" in
        assert_equal expected actual);
      (* TODO: write more tests *)
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
