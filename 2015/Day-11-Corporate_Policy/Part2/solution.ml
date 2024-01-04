let password = Hashtbl.create 10

let read_password () =
  let s = read_line () in
  String.iteri (fun id c -> Hashtbl.add password id (Char.code c)) s;
  String.length s - 1

let build_password last_char_id =
  String.init (last_char_id + 1) (fun i -> Char.chr (Hashtbl.find password i))

let increase_password last_char =
  let rec aux char_id ids =
    let code = Hashtbl.find password char_id in
    match code with
    | 104 | 107 | 110       -> Hashtbl.replace password char_id (code + 2); char_id::ids  (* Rule 2 *)
    | 122 when char_id = 0  -> (Hashtbl.replace password char_id 97; Hashtbl.add password (last_char + 1) 97; (char_id::ids) @ [last_char])
    | 122                   -> (Hashtbl.replace password (char_id) 97; aux (char_id - 1) (char_id::ids))
    | _                     -> (Hashtbl.replace password char_id (code + 1); char_id::ids)
  in
  aux last_char [] 
  
let rec respects_rule1 id =
  try
    let f = Hashtbl.find password id in
    let s = Hashtbl.find password (id+1) in
    let t = Hashtbl.find password (id+2) in
    match f,s,t with
    | (a, b, c) when a+1=b && a+2=c -> true
    | _                         -> respects_rule1 (id+1)
  with Not_found -> false

let rec respects_rule2 id =
  let rec find_pair id =
    try
      let f = Hashtbl.find password id in
      let s = Hashtbl.find password (id+1) in
      match f,s with
      | (a, b) when a=b -> (id+1, f)
      | _               -> find_pair (id+1)
    with Not_found -> (-1,-1)
  in
  let fst_pair = find_pair 0 in
  let snd_pair = find_pair (fst fst_pair) in
  match (fst fst_pair), (fst snd_pair) with
  | a, b when a <> -1 && b <> -1 && ((snd fst_pair) <> (snd snd_pair)) -> true
  | _ -> false
  
let rec find_next_valid_password last_char =
  let new_password  = increase_password last_char in
  let new_last_char = (List.nth new_password (List.length new_password - 1)) in
  if (respects_rule1 0) && (respects_rule2 0)
  then new_last_char
  else find_next_valid_password new_last_char

  
let () =
  let last_char_id  = read_password () in
  let new_last_char = find_next_valid_password last_char_id in
  let new_last_char = find_next_valid_password new_last_char in
  build_password new_last_char |> print_endline
