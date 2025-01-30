open Stdlib;; 

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
let min_sos n =
  if n < 0 then failwith "Undefined for negative numbers"
  else
    let dp = Array.make (n + 1) (n + 1) in
    dp.(0) <- 0; (* Base case: 0 requires 0 squares *)
    for i = 1 to n do
      let j = ref 1 in
      while !j * !j <= i do
        dp.(i) <- min dp.(i) (1 + dp.(i - (!j * !j)));
        j := !j + 1
      done
    done;
    dp.(n);;

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



