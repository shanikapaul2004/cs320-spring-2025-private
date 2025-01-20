open Assign0

(* Comment out individual lines in order to "turn off" a test case *)

let sqrt_tests (_ : unit) : unit list =
  [
    (* assert (sqrt 4 = 2); *)
    assert (sqrt 9 = 3);
    assert (sqrt 100 = 10);
    assert (sqrt 2 = 2);
    assert (sqrt 10 = 4);
    assert (sqrt 99 = 10);
  ]

let is_prime_tests (_ : unit) : unit list =
  [
    assert (is_prime 2);
    assert (is_prime 37);
    assert (is_prime 97);
    assert (not (is_prime 0));
    assert (not (is_prime 1));
    assert (not (is_prime 57));
  ]

let _run_tests : unit list list =
  [
    sqrt_tests ();
    is_prime_tests ();
  ]
