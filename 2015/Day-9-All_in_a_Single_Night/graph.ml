type 'a vertex  = 'a
type 'a edge    = {v1 : 'a vertex; v2 : 'a vertex; cost : int}
type 'a graph   = { vertex : ('a vertex) list; edges : ('a edge) list}

let create_graph = {vertex = []; edges = []}

let add_edge e graph = 
  {vertex = graph.vertex; edges = (e::(graph.edges))}

let add_vertex v graph = 
  {vertex = (v::graph.vertex); edges = graph.edges}

let remove_edge e graph = 
  {vertex = graph.vertex; edges = (List.filter (fun x -> x<>e) graph.edges)}

let remove_vertex v graph = 
  let new_vertex = List.filter (fun x -> x<>v) graph.vertex in
  let new_edges = List.filter (fun e -> ((e.v1 <> v) && (e.v2 <> v))) graph.edges in
  {vertex = new_vertex; edges = new_edges}

