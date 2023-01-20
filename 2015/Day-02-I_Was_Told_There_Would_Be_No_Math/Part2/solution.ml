let read_input () =
  let measures = read_line () in
  let measures = String.split_on_char 'x' measures in
  Array.of_list (List.map float_of_string measures)

let rec sum_file acc =
  try
    let package = read_input () in
    Array.fast_sort compare package;
    let wrap_ribbon = 2. *. package.(0) +. 2. *. package.(1) in
    let bow = package.(0) *. package.(1) *. package.(2) in
    sum_file (acc +. wrap_ribbon +. bow)
  with _ -> acc


let main () = 
  print_float (sum_file 0.);
  print_newline ()

let () = main ()
