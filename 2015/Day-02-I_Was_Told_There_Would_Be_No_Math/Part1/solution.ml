let read_input () =
  let measures = read_line () in
  let measures = String.split_on_char 'x' measures in
  Array.of_list (List.map float_of_string measures)

let rec sum_file acc =
  try
    let package = read_input () in
    let rect1 = package.(0) *. package.(1) in
    let rect2 = package.(0) *. package.(2) in
    let rect3 = package.(1) *. package.(2) in
    let package_area = 2.*.rect1 +. 2.*.rect2 +. 2.*.rect3 +. (min rect1 (min rect2 rect3)) in
    sum_file (acc +. package_area)
  with _ -> acc


let main () = 
  print_float (sum_file 0.);
  print_newline ()

let () = main ()
