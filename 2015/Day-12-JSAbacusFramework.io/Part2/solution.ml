let add_json json =
  let rec has_red l = 
    List.fold_left 
      (fun acc (s,j ) ->
        acc + match j with 
        | `Assoc _  -> 0
        | `Bool _   -> 0
        | `Float _  -> 0
        | `Int _    -> 0
        | `List _   -> 0
        | `Null     -> 0
        | `String s -> if String.equal s "red" then 1 else 0 
      )
      0
      l
  in  
  let rec aux = function
    | `Assoc l  -> 
      if has_red (l) == 0 
      then List.fold_left (fun acc (s, j) -> acc + aux j) 0 l 
      else 0
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