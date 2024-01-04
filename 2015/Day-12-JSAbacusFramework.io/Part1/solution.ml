let add_json json =
  let rec aux = function
    | `Assoc l  -> List.fold_left (fun acc (s, j) -> acc + aux j) 0 l
    | `Bool _   -> 0
    | `Float _  -> 0
    | `Int i    -> i
    | `List l   -> List.fold_left (fun acc j -> acc + aux j) 0 l 
    | `Null     -> 0
    | `String _ -> 0
  in
  aux json
  
let () =
  Yojson.Basic.from_file "../input.json" |> add_json |> print_int |> print_newline
  
  