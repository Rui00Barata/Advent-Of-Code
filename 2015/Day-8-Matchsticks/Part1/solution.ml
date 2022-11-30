let iter_strings () = 
  let rec aux code memory = 
    try
      let s = read_line () in
      let size = String.length s in
      let new_s = String.sub s 1 (size-2) in
      let new_s = Scanf.unescaped new_s in
      let new_size = String.length new_s in
      aux (code+size) (memory+new_size)
    with _ -> (code, memory)
  in
  aux 0 0

let code, memory = iter_strings ()

let () = print_int (code-memory); print_newline ()

