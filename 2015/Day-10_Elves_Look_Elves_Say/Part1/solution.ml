let iter_string s =
  let new_string = Buffer.create ((String.length s) * 2) in
  let rec aux last_c count pos =
    try
      match s.[pos] with
      | curr_c when last_c = curr_c -> aux last_c (count+1) (pos+1)
      | curr_c                      -> 
          Buffer.add_char new_string (char_of_int (count+48)); 
          Buffer.add_char new_string last_c; 
          aux curr_c 1 (pos+1)
    with _ -> 
      Buffer.add_char new_string (char_of_int (count+48)); 
      Buffer.add_char new_string last_c; 
      Buffer.contents new_string
  in
  aux s.[0] 1 1


let apply s =
  let rec aux idx s = 
    if idx = 40 then s 
    else aux (idx+1) (iter_string s)
  in
  aux 0 s


let s = read_line ()
let () = Printf.printf "%d\n" (String.length (apply s))

