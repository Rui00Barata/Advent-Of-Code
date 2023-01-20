type 'a vertex  = 'a
type 'a edge    = {v1 : 'a vertex; v2 : 'a vertex; cost : int}


let add_edge e graph = 
  graph.(e.v1).(e.v2) <- e.cost;
  graph.(e.v2).(e.v1) <- e.cost


let create_graph edges n_vertex = 
  let get_edge i j =
    let rec match_j = function
      | []                        -> -1
      | (v2, cost)::t when v2 = j -> cost
      | h::t                      -> match_j t
    in
    match_j (Hashtbl.find_all edges i) 
  in
  Array.init n_vertex (fun i -> Array.init n_vertex (fun j -> if i=j then 0 else get_edge i j))


let read_input () = 
  let edges   = Hashtbl.create 25 in
  let vertex  = Hashtbl.create 25 in
  let translate_vertex v ni =
    match Hashtbl.find_opt vertex v with
    | None    -> Hashtbl.add vertex v ni; (ni+1)
    | Some n  -> ni
  in
  let rec aux next_index = 
    try
      let s     = read_line () in
      let input = String.split_on_char ' ' s in
      let input = Array.of_list input in
      let ni    = translate_vertex (input.(0)) next_index in 
      let ni    = translate_vertex (input.(2)) ni in 
      let cost  = int_of_string (input.(4)) in 
      Hashtbl.add edges (Hashtbl.find vertex input.(0))  ((Hashtbl.find vertex input.(2)), cost);
      Hashtbl.add edges (Hashtbl.find vertex input.(2))  ((Hashtbl.find vertex input.(0)), cost);
      aux ni
    with _ -> () 
  in 
  aux 0;
  create_graph edges (Hashtbl.length vertex), vertex


let rec perm lst =
  let rec insert x lst =
    match lst with
    | [] -> [[x]]
    | h::t -> 
      (x::lst) :: (List.map (fun el -> h::el) (insert x t))
  in
  match lst with
  | [] -> [lst]
  | h::t -> 
    List.flatten (List.map (insert h) (perm t))


let path_to_cost graph path =
  let rec translate_path cost = function
    | v1::v2::[]  -> graph.(v1).(v2) + cost
    | v1::v2::t   -> translate_path (graph.(v1).(v2) + cost) (v2::t)
    | _ -> raise (Invalid_argument "Path to short")
  in
  translate_path 0 path


let find_best_path paths graph =
  List.hd (List.fast_sort compare (List.map (path_to_cost graph) paths))

    
let main () =
  let graph, vertex = read_input () in
  let vertex = List.of_seq (Hashtbl.to_seq_values vertex) in
  let paths = perm vertex in
  print_int (find_best_path paths graph)


let () = main ()
