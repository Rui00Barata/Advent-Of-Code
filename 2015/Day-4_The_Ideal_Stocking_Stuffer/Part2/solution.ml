let rec match_string s idx = 
  if idx > 5 then true
  else if s.[idx] = '0' then match_string s (idx+1)
  else false

let rec find_answer s n = 
  let md5 = Digest.string (s^(string_of_int n)) in
  let md5_text = Digest.to_hex md5 in
  if match_string md5_text 0 then n
  else find_answer s (n+1)
  
let s = read_line () 

let () = print_int (find_answer s 0)
let () = print_newline ()