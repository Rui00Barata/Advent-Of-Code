open Graph

let read_input () = 
  let rec aux g = 
    try
      let s = read_line () in
      let input = String.split_on_char ' ' s in
      let input = Array.of_list input in
      let v1 = input.(0) in 
      let v2 = input.(2) in
      let cost = input.(4) in 
      let n_edge = {v1 = v1; v2 = v2; cost = cost} in 
      aux (add_edge n_edge g)
    with _ -> g 
  in 
  aux (create_graph ())



let g = read_input ()

let imp =
  let v         = List.length g.vertex in 
  let cost = Array.make_matrix  v v max_int in 
  let p_queue = [] in
  for i = 0 to v-1 do 
    
