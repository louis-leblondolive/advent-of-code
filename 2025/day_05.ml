let read_input file = 

  let rec get_intervals file = 
    match input_line file with 
    | str -> if str = "" then []
             else Scanf.sscanf str "%d-%d" (fun a b -> let l = get_intervals file in (a, b) :: l)
    | exception End_of_file -> []
  in

  let rec get_fresh_products file =
    match input_line file with 
    | str -> Scanf.sscanf str "%d" (fun n -> let l = get_fresh_products file in n :: l)
    | exception End_of_file -> []
  in 

  let intervals = get_intervals file in 
  let fresh_products = get_fresh_products file in 

  (intervals, fresh_products)


let puzzle_01 intervals products = 

  let rec count_fresh_products cpt products = 
    match products with 
    | [] -> cpt
    | pdct :: tl -> if List.fold_left (fun fresh (a, b) -> fresh || a <= pdct && pdct <= b) false intervals 
                    then count_fresh_products (cpt + 1) tl
                    else count_fresh_products cpt tl
  in

  count_fresh_products 0 products
      

let puzzle_02 intervals = 

  let sorted_infs = List.sort (fun (a, _) (b, _) -> a - b) intervals in 
  
  let max a b = 
    if a > b then a else b 
  in


  let rec merge_intervals new_intervals current_inf current_sup sorted_infs =

    match sorted_infs with 
    | [] -> (current_inf, current_sup) :: new_intervals

    | (inf, sup) :: tl -> if inf <= current_sup + 1 then 
                      let new_sup = max sup current_sup in
                      merge_intervals new_intervals current_inf new_sup tl 
                   else 
                      merge_intervals ((current_inf, current_sup) :: new_intervals) 
                                    inf sup tl 
  in
  
  match sorted_infs with 
  | [] -> failwith "no data"
  | (a, b) :: tl -> let new_intervals = merge_intervals [] a b tl in
               List.iter (fun (a, b) -> Printf.printf "%d - %d\n" a b) new_intervals;
               
               List.fold_left (fun cpt (a,b) -> cpt + b - a + 1) 0 new_intervals











let main () = 
  let file = open_in "input_05.txt" in 
  let (intervals, fresh_products) = read_input file in 

  (* List.iter (fun (a,b) -> Printf.printf "%d-%d\n" a b) intervals ;  *)
  (* List.iter (Printf.printf "%d\n") fresh_products  *)

  Printf.printf "Puzzle 01 : %d fresh products \n" (puzzle_01 intervals fresh_products) ;
  Printf.printf "Puzzle 02 : %d fresh products \n" (puzzle_02 intervals)


let () = main ()