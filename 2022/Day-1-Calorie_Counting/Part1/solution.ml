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
  

let find_max_calories () =
  let n_elfs = Hashtbl.length food_list in
  let rec aux id max_food =
    if id > n_elfs then
      max_food
    else
      let elf_food = Hashtbl.find_all food_list id in
      let total_elf_food = get_total_food elf_food in
      aux
        (id+1) 
        (if total_elf_food > max_food then total_elf_food
        else max_food)
  in
  aux 1 0
    

let () = read_input () 
let () = print_int (find_max_calories ()); print_newline ()
