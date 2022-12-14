let food_list = Hashtbl.create 20


let read_input () =  
  let rec aux id = 
    try
      Hashtbl.add food_list id (read_int ());
      aux id
    with 
    | Failure _   -> aux (id+1)
    | End_of_file -> ()
  in
  aux 1


let get_total_food l =
  List.fold_left (+) 0 l 
  

let index_calories () =
  let n_elfs = Hashtbl.length food_list in
  let rec aux id calories_list =
    if id > n_elfs then
      calories_list
    else
      let elf_food = Hashtbl.find_all food_list id in
      let total_elf_food = get_total_food elf_food in
      aux (id+1) (total_elf_food::calories_list)
  in
  aux 1 []


let get_3_first_elfs l = 
  let rec aux id cl acc =
    if id = 3 then
      acc
    else
      match cl with
      | []    -> raise (Failure "To few arguments.")
      | h::t  -> aux (id+1) t (acc+h)
  in
  aux 0 l 0
    

let compare a b = 
  b-a 


let () = 
  read_input ();
  let calories_list = index_calories () in
  let calories_list = List.fast_sort compare calories_list in
  print_int (get_3_first_elfs calories_list);
  print_newline ()
