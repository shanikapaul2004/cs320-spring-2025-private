open Assign2

let testing = true (* switch this flag to turn off testing globally *)

let run cases b = if b then cases () else []

let convert_tests () =
  let test_in = [Int 2; Int 3; String "a"; String "b"; Int 4; String "c"] in
  let test_out = [IntList [2;3]; StringList ["a";"b"]; IntList [4]; StringList ["c"]] in
  [
    assert (convert test_in = test_out);
  ]

let recipes_tests () =
  let r1 = { name = "1" ; ingrs = ["a"; "b"; "d"] } in
  let r2 = { name = "2" ; ingrs = ["a"; "c"; "e"] } in
  let r3 = { name = "3" ; ingrs = ["b"; "c"] } in
  [
    assert (recipes_by_ingrs [r1;r2;r3] ["a";"b";"c";"d"] = [r1;r3]);
    assert (recipes_by_ingrs [r1;r2;r3] ["a";"b";"c";"e"] = [r2;r3]);
  ]

let allocate_tests () =
  [
    assert (allocate 3 [] = Success (0, [Occupied, 3]));
    assert (allocate (-3) [(Occupied, 10); (Free, 2)] = Invalid_size);
    assert (allocate 4 [(Occupied, 1); (Free, 5); (Occupied, 3)]
            = Success (1, [(Occupied, 1); (Occupied, 4); (Free, 1); (Occupied, 3)]));
  ]

let free_tests () =
  [
    assert (free 3 [(Occupied, 3); (Occupied, 5); (Occupied, 3)]
            = Success [(Occupied, 3); (Free, 5); (Occupied, 3)]);
    assert (free 3 [(Occupied, 3); (Occupied, 5); (Free, 3); (Occupied, 3)]
            = Success [(Occupied, 3); (Free, 8); (Occupied, 3)]);
    assert (free 8 [(Occupied, 3); (Occupied, 5); (Occupied, 3)]
            = Success [(Occupied, 3); (Occupied, 5)]);
    assert (free (-1) [(Occupied, 3); (Occupied, 5); (Occupied, 3)] = Invalid_position);
    assert (free 2 [(Occupied, 3); (Occupied, 5); (Occupied, 3)] = Invalid_position);
    assert (free 3 [(Occupied, 3); (Free, 5); (Occupied, 3)] = Invalid_position);
  ]

let _run_tests : unit list list =
  if not testing then [] else
    [
      run convert_tests true;
      run recipes_tests true;
      run allocate_tests true;
      run free_tests true;
    ]
