let min (compare: 'a -> 'a -> int) (x: 'a) (y: 'a) =
  if compare x y >= 0
  then y
  else x

let max (compare: 'a -> 'a -> int) (x: 'a) (y: 'a) =
  if compare x y >= 0
  then x
  else y

type day = Mo | Tu | We | Th | Fr | Sa | Su

let string_of_day (d : day) : string =
  match d with
  | Mo -> "Mo"
  | Tu -> "Tu"
  | We -> "We"
  | Th -> "Th"
  | Fr -> "Fr"
  | Sa -> "Sa"
  | Su -> "Su"

let day_of_string_opt (s : string) : day option =
  match s with
  | "Mo" -> Some Mo
  | "Tu" -> Some Tu
  | "We" -> Some We
  | "Th" -> Some Th
  | "Fr" -> Some Fr
  | "Sa" -> Some Sa
  | "Su" -> Some Su
  | _ -> None

let int_of_day (d : day) : int =
  match d with
  | Mo -> 0
  | Tu -> 1
  | We -> 2
  | Th -> 3
  | Fr -> 4
  | Sa -> 5
  | Su -> 6

let compare_day (d1 : day) (d2 : day) : int =
  Int.compare (int_of_day d1) (int_of_day d2)

type hour = int

let string_of_hour (h : hour) : string =
  (if h < 10 then "0" else "") ^ string_of_int h

let hour_of_string_opt (s : string) : hour option =
  match int_of_string_opt s with
  | Some h ->
     if h >= 0 && h <= 24
     then Some h
     else None
  | None -> None

let compare_hour : hour -> hour -> int = Int.compare

type minute = M00 | M15 | M30 | M45

let string_of_minute (m : minute) : string =
  match m with
  | M00 -> "00"
  | M15 -> "15"
  | M30 -> "30"
  | M45 -> "45"

let minute_of_string_opt (s : string) : minute option =
  match s with
  | "00" -> Some M00
  | "15" -> Some M15
  | "30" -> Some M30
  | "45" -> Some M45
  | _ -> None

let int_of_minute (m : minute) : int =
  match m with
  | M00 -> 0
  | M15 -> 1
  | M30 -> 2
  | M45 -> 3

let compare_minute (m1 : minute) (m2 : minute) : int =
  Int.compare (int_of_minute m1) (int_of_minute m2)

type time =
  {
    hour : int;
    minute : minute;
  }

let time_of_string_opt (s : string) : time option =
  if String.length s = 5 && s.[2] = ':'
  then
    let h = hour_of_string_opt (String.sub s 0 2) in
    let m = minute_of_string_opt (String.sub s 3 2) in
    match h, m with
    | Some h, Some m ->
       if h = 24 && m <> M00
       then None
       else Some {hour=h;minute=m}
    | _ -> None
  else None

let string_of_time (t : time) : string =
  string_of_hour t.hour
  ^ ":"
  ^ string_of_minute t.minute

let compare_time (t1 : time) (t2 : time) : int =
  let ch = compare_hour t1.hour t2.hour in
  if ch = 0
  then compare_minute t1.minute t2.minute
  else ch

type interval =
  {
    day : day;
    start_time : time;
    end_time : time;
  }

let string_of_interval (i : interval) : string =
  string_of_day i.day
  ^ "  "
  ^ string_of_time i.start_time
  ^ "--"
  ^ string_of_time i.end_time

type schedule = interval list

let is_empty (i : interval) : bool = assert false

let compare_interval (i1 : interval) (i2 : interval) : int = assert false

let intersect_i_i (i1: interval) (i2: interval) : interval option = assert false

let intersect_s_i (s : schedule) (i: interval) : schedule = assert false

let intersect_s_s (s1 : schedule) (s2: schedule) : schedule = assert false

let intersect_schedules (s : schedule list) : schedule = assert false

let interval_of_string_opt (s : string) : interval option = assert false
