type rps      = | Rock | Paper | Scissors


let play_score = function
  | Rock      -> 1
  | Paper     -> 2
  | Scissors  -> 3


let play_match opponent me = 
  match opponent,me with
  | Rock, Rock          -> 3
  | Rock, Paper         -> 6
  | Rock, Scissors      -> 0
  | Paper, Rock         -> 0
  | Paper, Paper        -> 3
  | Paper, Scissors     -> 6
  | Scissors, Rock      -> 6
  | Scissors, Paper     -> 0
  | Scissors, Scissors  -> 3


let calculate_round_score opponent me =
  let opponent =
    match opponent with
    | 'A' -> Rock
    | 'B' -> Paper
    | _   -> Scissors
  in
  let me =
    match me with
    | 'X' -> Rock
    | 'Y' -> Paper
    | _   -> Scissors
  in
  (play_score me) + (play_match opponent me)


let calculate_total_score () =
  let rec aux acc = 
    try
      let s = read_line () in
      let opponent,me = s.[0], s.[2] in
      aux (acc+calculate_round_score opponent me)
    with _ -> acc
  in
  aux 0
  

let () =
  print_int (calculate_total_score ()); 
  print_newline ()

