open Unsigned

type value = 
  | Volt of Unsigned.UInt16.t 
  | Name of string

type opp = 
  | Wire            of (value * value)
  | And             of (value * value * value)
  | Or              of (value * value * value)
  | Lshift          of (value * int *  value )
  | Rshift          of (value * int *  value )
  | Not             of (value * value )


let circuit = Hashtbl.create 100


let string_to_value s =
  try 
    Volt (Unsigned.UInt16.of_int (int_of_string s))
  with _ -> Name (s)


let value_to_string = function
  | Volt v  -> Unsigned.UInt16.to_int (v)
  | _       -> 0


let rec build_circuit ()= 
  let wire_regex            = Str.regexp "\\([A-Za-z0-9]+\\) -> " in
  let and_regex             = Str.regexp "\\([A-Za-z0-9]+\\) \\(AND\\)+" in
  let or_regex              = Str.regexp "\\([A-Za-z0-9]+\\) \\(OR\\)+" in
  let lshift_regex          = Str.regexp "\\([A-Za-z0-9]+\\) \\(LSHIFT\\)+" in
  let rshift_regex          = Str.regexp "\\([A-Za-z0-9]+\\) \\(RSHIFT\\)+" in
  let translate s =
    let sl = String.split_on_char ' ' s in
    let sl = Array.of_list sl in
    let sl = Array.map string_to_value sl in
    if Str.string_match wire_regex s 0 then (sl.(2), Wire (sl.(2), sl.(0)))
    else if Str.string_match and_regex s 0 then (sl.(4), And (sl.(4), sl.(2), sl.(0)))
    else if Str.string_match or_regex s 0 then (sl.(4), Or (sl.(4), sl.(2), sl.(0)))
    else if Str.string_match lshift_regex s 0 then (sl.(4), Lshift (sl.(4), value_to_string sl.(2), sl.(0)))
    else if Str.string_match rshift_regex s 0 then (sl.(4), Rshift (sl.(4), value_to_string sl.(2), sl.(0)))
    else (sl.(3), Not (sl.(3), sl.(1)))
  in
  try
    let s = read_line () in
    let k, piece = translate s in
    Hashtbl.add circuit k piece;
    build_circuit ()
  with _ -> ()


let rec find_circuit_output output = 
  let get_voltage w = 
    match w with
    | Volt v  -> v
    | _       -> find_circuit_output w 
  in
  try
(*
    let a = match output with |Name n -> n | _ -> "" in
    print_endline a;
*)
    let new_v = 
      match Hashtbl.find circuit output with
      | Wire (o, v)       -> get_voltage v 
      | And (o, y, x)     -> let y = get_voltage y in let x = get_voltage x in Unsigned.UInt16.logand y x 
      | Or (o, y ,x)      -> let y = get_voltage y in let x = get_voltage x in Unsigned.UInt16.logor y x
      | Lshift (o, v, x)  -> let x = get_voltage x in Unsigned.UInt16.shift_left x v 
      | Rshift (o, v, x)  -> let x = get_voltage x in Unsigned.UInt16.shift_right x v 
      | Not (o, x)        -> let x = get_voltage x in Unsigned.UInt16.lognot x
    in
    Hashtbl.add circuit output (Wire (output, (Volt new_v)));
    new_v
  with _ -> raise Not_found 

let () = build_circuit ()

let () = Hashtbl.add circuit (Name "b") (Wire (Name "b", Volt (Unsigned.UInt16.of_int 956))) 

let a = Unsigned.UInt16.to_int (find_circuit_output (Name "a"))

let () = print_int a; print_newline ()

