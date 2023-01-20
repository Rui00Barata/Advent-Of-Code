let lights = Array.make_matrix 1000 1000 0

let parse_input () =
  let get_coords s =
    let s = String.split_on_char ',' s in
    let s = List.map int_of_string s in 
    let x::y::_ = s in
    (x,y)
  in
  let input = String.split_on_char ' ' (read_line ()) in
  let input = Array.of_list input in
  if input.(0) = "toggle" then 
    let (x1,y1) = get_coords input.(1) in
    let (x2,y2) = get_coords input.(3) in
    (0,(x1,y1),(x2,y2))
  else 
    let (x1,y1) = get_coords input.(2) in
    let (x2,y2) = get_coords input.(4) in
    if input.(1) = "on" then
      (1,(x1,y1),(x2,y2))
    else 
      (2,(x1,y1),(x2,y2))

let follow_instructions (t,(x1,y1),(x2,y2)) = 
  let toggle (x1,y1) (x2,y2) = 
    for i=x1 to x2 do
      for j = y1 to y2 do
        match lights.(i).(j) with
        | 0 -> lights.(i).(j) <- 1
        | _ -> lights.(i).(j) <- 0
      done
    done
  in
  let turn_on (x1,y1) (x2,y2) =
    for i=x1 to x2 do
      for j = y1 to y2 do
       lights.(i).(j) <- 1
      done
    done
  in
  let turn_off (x1,y1) (x2,y2) =
    for i=x1 to x2 do
      for j = y1 to y2 do
       lights.(i).(j) <- 0
      done
    done
  in
  match t with
  | 0 -> toggle (x1,y1) (x2,y2)
  | 1 -> turn_on (x1,y1) (x2,y2)
  | _ -> turn_off (x1,y1) (x2,y2)

let count_turned_on () = 
  Array.fold_left (fun acc al -> (Array.fold_left (fun acc1 x -> (if x=1 then 1 else 0) + acc1) 0 al) + acc) 0 lights


let () = 
  try 
    while true do
      let instructions = parse_input () in
      follow_instructions instructions
    done
  with _ -> (print_int (count_turned_on ()); print_newline ()) 

