module IntTuple = struct
  type t = int * int

  let compare (a,b) (c,d) = 
    let fst = compare a c in
    if fst <> 0 then fst
    else 
      compare b d 
end

module SIT = Set.Make(IntTuple)

let rec follow_instructions sit s idx (x,y) = 
  try
    let (x,y) = 
      match s.[idx] with
      | '^' -> (x+1,y) 
      | 'v' -> (x-1,y) 
      | '>' -> (x,y+1) 
      | _   -> (x,y-1) 
    in
    let sit = SIT.add (x,y) sit in 
    follow_instructions sit s (idx+1) (x,y)
  with _ -> sit

let s = read_line () 
let sit = SIT.empty 
let sit = SIT.add (0,0) sit 
let sit = follow_instructions sit s 0 (0,0)

let () = print_int (SIT.cardinal sit)
let () = print_newline ()

