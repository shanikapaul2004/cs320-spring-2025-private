open Interp2
open OUnit2

let desugar_tests =
  "desugar test suite" >:::
    [
      "basic test" >:: (fun _ ->
        let prog =
          Option.get
            (parse
               "let id (x : int) : int = x
                let y : int = 5")
        in
        let expected =
          Let
            {
              is_rec = false;
              name = "id";
              ty = FunTy (IntTy, IntTy);
              binding = Fun ("x", IntTy, Var "x");
              body = Let
                       {
                         is_rec = false;
                         name = "y";
                         ty = IntTy;
                         binding = Num 5;
                         body = Var "y";
                       };
            }
        in
        let actual = desugar prog in
        assert_equal expected actual)
    ]

(* Type Checker Tests *)
let type_of_tests =
  "type_of test suite" >:::
    [
      "basic test" >:: (fun _ ->
        let expr = Fun ("x", BoolTy, Num 5) in
        let expected = Ok (FunTy (BoolTy, IntTy)) in
        let actual = type_of expr in
        assert_equal expected actual)
    ]

(* Interp Tests *)
let interp_tests =
  "interp test suite" >:::
    [
      "basic eval test" >:: (fun _ ->
        let result = interp "let _ : int = 2 + 3" in
        assert_equal (Ok (VNum 5)) result);

      "assert fail test" >:: (fun _ ->
        let result = interp "let _ : unit = assert (false)" in
        assert_equal (Error (AssertTyErr BoolTy)) result);

      "division by zero test" >:: (fun _ ->
        let result = interp "let _ : int = 1 / 0" in
        assert_equal (Error (OpTyErrR (Div, IntTy, IntTy))) result);

      "unbound variable test" >:: (fun _ ->
        let result = interp "let _ : int = x + 1" in
        assert_equal (Error (UnknownVar "x")) result);

      "function app type error" >:: (fun _ ->
        let result = interp "let _ : int = 3(4)" in
        assert_equal (Error (FunAppTyErr IntTy)) result);

      "recursion test" >:: (fun _ ->
        let result =
          interp
            "let rec f : int -> int = fun (x : int) ->
               if x <= 1 then 1 else x * f (x - 1)
             let _ : int = f 5"
        in
        assert_equal (Ok (VNum 120)) result);
    ]


let tests =
  "interp2 test suite" >:::
    [
      desugar_tests;
      type_of_tests;
      interp_tests;
    ]

let _ = run_test_tt_main tests
