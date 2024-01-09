open Hashtbl

type connection = { happiness : int; person1 : int; person2 : int}


let ids = create 10 
(* iter (fun a b -> Printf.printf "%s: %d\n" a b) ids;; *)
let connections = create 10 
(* iter (fun (a,b) v -> Printf.printf "(%d,%d): %d)\n" a b v) connections;; *)


let get_info_from_line l =
  let rec aux id value person1 = function
    | []                -> raise (Failure "Unreachable")
    | [e]               -> person1, e, value
    | h::t when id=0    -> aux (id+1) value h t
    | s::v::t when id=2 -> 
      let v = int_of_string v in
      aux (id+1) (if String.equal "gain" s then v else -v)  person1 t
    | h::t              -> aux (id+1) value person1 t
  in
  aux 0 0 "" l 

let read_input () =
  let next_id = ref 0 in
  try
    while true do
      let s = read_line () in
      let s = String.sub s 0 (String.length s - 1) in
      let sl = String.split_on_char ' ' s in
      let p1, p2, v = get_info_from_line sl in
      let p1 = 
        match find_opt ids p1 with
        | None    -> add ids p1 !next_id; incr next_id; (!next_id - 1)
        | Some id -> id
      in
      let p2 = 
        match find_opt ids p2 with
        | None    -> add ids p2 !next_id; incr next_id; (!next_id - 1)
        | Some id -> id
      in
      match find_opt connections (min p1 p2, max p1 p2) with
      | None    -> add connections (min p1 p2, max p1 p2) v
      | Some old_v  -> replace connections (min p1 p2, max p1 p2) (old_v+v)
    done;
    !next_id  
  with End_of_file -> !next_id
    
    
let find_max_happiness size =
  let connection_list = 
    fold 
      (fun (p1, p2) v cl -> {happiness=v; person1=p1; person2=p2}::cl) 
      connections 
      [] 
    |> List.fast_sort (fun a b -> if a.happiness > b.happiness then -1 else if a.happiness < b.happiness then 1 else 0)
  in
  let available_seats = Array.make size 2 in
  let rec fill_seats connections happiness nas = 
    if nas = 0 then happiness
    else
      match connections with
      | []    -> raise (Failure "Unreachable 1")
      | c::t  -> 
        if (available_seats.(c.person1) > 0) && (available_seats.(c.person2) > 0) then (
          available_seats.(c.person1) <- available_seats.(c.person1) - 1;
          available_seats.(c.person2) <- available_seats.(c.person2) - 1;
          fill_seats t (happiness+c.happiness) (nas-1))
        else
          fill_seats t happiness nas
  in
  fill_seats connection_list 0 size

  
let () =
  read_input () |> find_max_happiness |> print_int |> print_newline

  