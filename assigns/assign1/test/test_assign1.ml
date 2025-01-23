open Assign1

(* You can put your own testing code up here if you want *)



(* ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ *)

let testing = true (* you can switch this flag to turn of testing globally *)

let run cases b = if b then cases () else []

let is_perfect_tests () =
  [
    assert (is_perfect 6); (* 1 + 2 + 3 = 6 *)
    assert (is_perfect 28); (* 1 + 2 + 4 + 7 + 14 = 28 *)
    assert (not (is_perfect 24)); (* 1 + 2 + 3 + 4 + 6 + 8 + 12 <> 24 *)
  ]

let min_sos_tests () =
  [
    assert (min_sos 1 = 1); (* 1^2 = 1 *)
    assert (min_sos 2 = 2); (* 1^2 + 1^2 = 2 *)
    assert (min_sos 3 = 3); (* 1^2 + 1^2 + 1^2 = 3 *)
    assert (min_sos 4 = 1); (* 2^2 = 4 *)
    assert (min_sos 8 = 2); (* 2^2 + 2^2 = 8 *)
  ]

let num_occurs_tests () =
  [
    assert (num_occurs ~sub:"AB" "ABABAB" = 3);
    assert (num_occurs ~sub:"AA" "AAAAAA" = 5);
    assert (num_occurs ~sub:"cow" "holy cow, what a cool cow" = 2);
  ]

let _run_tests : unit list list =
  if not testing then [] else
  [
    run is_perfect_tests true;
    run min_sos_tests true;
    run num_occurs_tests true;
  ]
