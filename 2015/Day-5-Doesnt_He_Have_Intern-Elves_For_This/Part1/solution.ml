let classify_string s =
  let rec aux idx vowels doublec =
    try
      let currentc = s.[idx] in
      let vowels = 
        match currentc with
        | 'a' | 'e' | 'i' | 'o' | 'u' -> vowels+1 
        | _                           -> vowels
      in
      try 
        let nextc = s.[idx+1] in
        match (currentc,nextc) with
        | ('a','b') | ('c','d') | ('p','q') | ('x','y') -> false
        | _                                             ->
          if not doublec then
            let doublec = (currentc = nextc) in
            aux (idx+1) vowels doublec
          else
            aux (idx+1) vowels doublec
      with _ -> aux (idx+1) vowels doublec
    with _ -> ((vowels>=3) && doublec)
  in
  aux 0 0 false

let rec count_nice_strings acc =
  try
    let s = read_line () in
    let acc = acc + if classify_string s then 1 else 0 in
    count_nice_strings acc;
  with _ -> acc

let () = print_int (count_nice_strings 0); print_newline ()

      