let instructions = read_line ()

let rec get_to_basement lvl idx =
	let lvl =
		match instructions.[idx-1] with
		| '('	-> lvl+1
		| _ 	-> lvl-1
	in
  if lvl < 0 then idx
	else get_to_basement lvl (idx+1) 

let () = print_int (get_to_basement 0 1); print_newline ()