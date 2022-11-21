module IntTuple = struct
  type t = int * int

  let compare (a,b) (c,d) = 
    let fst = compare a c in
    if fst <> 0 then fst
    else 
      compare b d 
end

module SIT = Set.Make(IntTuple)

let rec follow_instructions sit s idx (santa_x, santa_y) (robot_x, robot_y) santa_time = 
  try
    if santa_time then
      let (santa_x, santa_y) = 
        match s.[idx] with
        | '^' -> (santa_x+1,santa_y) 
        | 'v' -> (santa_x-1,santa_y) 
        | '>' -> (santa_x,santa_y+1) 
        | _   -> (santa_x,santa_y-1) 
      in
      let sit = SIT.add (santa_x,santa_y) sit in 
      follow_instructions sit s (idx+1) (santa_x, santa_y) (robot_x, robot_y) false
    else
      let (robot_x, robot_y) =
        match s.[idx] with
        | '^' -> (robot_x+1,robot_y) 
        | 'v' -> (robot_x-1,robot_y) 
        | '>' -> (robot_x,robot_y+1) 
        | _   -> (robot_x,robot_y-1) 
      in
      let sit = SIT.add (robot_x,robot_y) sit in 
      follow_instructions sit s (idx+1) (santa_x, santa_y) (robot_x, robot_y) true
  with _ -> sit

let s = read_line () 
let sit = SIT.empty 
let sit = SIT.add (0,0) sit 
let sit = follow_instructions sit s 0 (0,0) (0,0) true

let () = print_int (SIT.cardinal sit)
let () = print_newline ()

