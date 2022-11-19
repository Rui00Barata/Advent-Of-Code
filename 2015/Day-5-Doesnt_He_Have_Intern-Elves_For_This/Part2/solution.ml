let containts s1 s2 id =
  let lenghts1 = String.length s1 in
  let rec aux id = 
    if (id > lenghts1-2) then false
    else 
      if ((s1.[id] = s2.[0]) && (s1.[id+1] = s2.[1])) then true
      else aux (id+1) in
  aux id

let check_rule1 s =
  let lengths = String.length s in 
  let rec aux id =
    if (id > lengths-2) then false
    else
      let cont = containts s (String.sub s id 2) (id+2) in
      if cont then true else aux (id+1)
  in
  aux 0
      

let check_rule2 s = 
  let lengths = String.length s in
  let rec aux previous_char idx = 
    if idx >= lengths then false 
    else
      if s.[idx] = previous_char then true else aux s.[idx-1] (idx+1)
  in
  aux s.[0] 2


let check_string s = 
  if check_rule2 s then 
    if check_rule1 s then 1
    else 0
  else 0


let rec count_nice_strings acc = 
  try
    let s = read_line () in
    count_nice_strings (acc + check_string s)
  with _ -> acc


let () = print_int (count_nice_strings 0); print_newline ()
