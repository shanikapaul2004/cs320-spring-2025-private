
(*problem 1*)
let is_perfect n =
  let rec sum_divisors d acc =
    if d = 0 then acc
    else if n mod d = 0 then sum_divisors (d - 1) (acc + d)
    else sum_divisors (d - 1) acc
  in
  if n <= 0 then failwith "Undefined for non-positive numbers"
  else sum_divisors (n / 2) 0 = n

(*problem 2: similar to coin change problem*)
let rec make_list n value =
  if n = 0 then [] else value :: make_list (n - 1) value;;
let rec update_list lst index new_value =
  match lst with
  | [] -> []
  | x :: xs ->
      if index = 0 then new_value :: xs
      else x :: update_list xs (index - 1) new_value;;

let min_sos n =
  if n < 0 then failwith "Undefined for negative numbers"
  else
    let rec update_dp dp i =
      if i > n then dp
      else
        let rec update_j dp j =
          if j * j > i then dp
          else
            let current_value = List.nth dp i in
            let new_value = min current_value (1 + List.nth dp (i - (j * j))) in
            let updated_dp = update_list dp i new_value in
            update_j updated_dp (j + 1)
        in
        update_dp (update_j dp 1) (i + 1)
    in
    let dp = make_list (n + 1) (n + 1) in
    let dp = update_list dp 0 0 in
    List.nth (update_dp dp 1) n;;


(*problem 3*)
let num_occurs ~sub:s t =
  let s_len = String.length s in
  let t_len = String.length t in
  let rec count_from pos acc =
    if pos > t_len - s_len then
      acc
    else
      let matches = 
        try 
          String.sub t pos s_len = s
        with Invalid_argument _ -> 
          false
      in
      count_from (pos + 1) (if matches then acc + 1 else acc)
  in
  count_from 0 0


