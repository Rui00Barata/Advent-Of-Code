let instructions = read_line ()

let follow_instructions lvl = function
	| '(' -> lvl + 1
	| _   -> lvl - 1

let () = print_int (String.fold_left follow_instructions 0 instructions); print_newline ()