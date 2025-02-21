open OUnit2
open Demo

let tests =
  "exactly_one test suite" >::: [
    "exactly_one on empty list" >:: (fun _ ->
      assert_equal (Error MoreThanOne) (exactly_one (fun _ -> true) []));
  ]

let _ = run_test_tt_main tests
