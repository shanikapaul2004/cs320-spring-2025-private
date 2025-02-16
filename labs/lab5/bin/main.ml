open Lab5

let lines (filename : string) =
  let rec loop acc ic =
    match input_line ic with
    | line -> loop (line :: acc) ic
    | exception End_of_file -> List.rev acc
  in
  In_channel.with_open_text filename (loop [])

let () =
  let folder intervals filename =
    filename
    |> lines
    |> List.map String.trim
    |> List.filter_map interval_of_string_opt
    |> (fun x -> x :: intervals)
  in
  Sys.argv
  |> Array.to_list
  |> List.tl
  |> List.fold_left folder []
  |> intersect_schedules
  |> List.sort compare_interval
  |> List.map string_of_interval
  |> (fun l -> if List.is_empty l then "No availability" else String.concat "\n" l)
  |> print_string
