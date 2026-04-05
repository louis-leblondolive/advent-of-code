type problem = {
  params : int list ;
  operator : string
}

let print_problem p = 
  List.iter (fun n -> Printf.printf "%d\n" n) p.params;
  print_string p.operator ;
  print_newline ()

let string_to_array s = 
  let n = String.length s in 
  let tab = Array.make n ' ' in 
  String.iteri (fun i c -> tab.(i) <- c) s  ;
  tab

let read_input_puzzle_01 file = 
  (* returns a list of problems *)

  let rec read_line file = 
    match input_line file with 
    | str -> let line = List.filter (fun s -> not (String.contains s ' ')) (String.split_on_char ' ' str) in 
             (List.filter (fun s -> s <> "") line) :: read_line file

    | exception End_of_file -> []
  in 

  let input = List.rev (read_line file) in 


  let rec build_problems operators parameters = 
    match operators with 
    | [] -> []

    | op :: tl -> let p = { operator = op ; 
                            params = List.map int_of_string (List.map List.hd parameters) } in 

                  p :: (build_problems tl (List.map List.tl parameters))
  in

  build_problems (List.hd input) (List.tl input)



let read_input_puzzle_02 file = 

  let rec read_line file = 
    match input_line file with 
    | str -> str :: read_line file
    | exception End_of_file -> []
  in


  let build_params_tab params = 
    (* init array *)
    let params_tab = Array.make (List.length params) [||] in 
    List.iteri (fun i e -> params_tab.(i) <- string_to_array e) params ;

    (* offset numbers *)
    let n = Array.length params_tab in 
    let m = Array.length params_tab.(0) in 

    for x = 0 to m - 1 do 
      for y = n - 1 downto 0 do 

        let pos_y = ref y in 
        while !pos_y < n - 1 && params_tab.(!pos_y).(x) <> ' ' && params_tab.(!pos_y + 1).(x) = ' ' do

          params_tab.(!pos_y + 1).(x) <- params_tab.(!pos_y).(x) ;
          params_tab.(!pos_y).(x) <- ' ' ; 
          incr pos_y
        done 
      done
    done ;
    params_tab
  in
  
  let input = List.rev (read_line file) in 
  let op_tab = List.hd input in 
  let params_tab = build_params_tab (List.rev (List.tl input)) in 

  (* testing input *)
  (* Array.iter (fun s -> Array.iter (fun c -> print_char c) s ; print_newline () ) params_tab;  *)
  (* print_string op_tab ;  *)
  (* print_newline () ; *)

  let get_problems_id op_tab = 
    let n = String.length op_tab in 
    let res = ref [] in 
    for i = 0 to n - 1 do 
      if op_tab.[i] = '+' || op_tab.[i] = '*' then res := i :: !res
    done;
    !res
  in

  let problem_from_id op start_id end_id params_tab = 

    let n = Array.length params_tab in 

    let prms = ref [] in 

    for i = start_id to end_id - 1 do 
      let exp = ref 1 in 
      let nb = ref 0 in 
      for j = n - 1 downto 0 do 
        if params_tab.(j).(i) <> ' ' then begin 
          nb := !nb + !exp * (Char.code params_tab.(j).(i) - Char.code '0') ;
          exp := !exp * 10 
        end 
      done ; 
      if !nb <> 0 then prms := !nb :: !prms 
    done ;

    (* Printf.printf "pb start %d end %d : " start_id end_id; *)
    (* List.iter (fun n -> Printf.printf "%d " n) !prms ; print_newline () ; *)

    { operator = String.make 1 op ; params = !prms}
  in

  let len_line = Array.length params_tab.(0) in 
  let rec build_problems pbs_ids params_tab = 
    match pbs_ids with 
    | [] -> []

    | [id] -> [problem_from_id  op_tab.[id] id len_line params_tab]

    | id_start :: id_end :: tl -> let pb = problem_from_id op_tab.[id_start] id_start id_end params_tab in 
                              pb :: build_problems (id_end :: tl) params_tab
  in

  build_problems (List.rev (get_problems_id op_tab)) params_tab 

    



let process_problems problems = 

  List.fold_left (fun total p -> if p.operator = "*" then total + (List.fold_left (fun a b -> a * b) 1 p.params)
                                 else total + (List.fold_left (+) 0 p.params))
                  0
                  problems




let main () = 

  let problems_puzzle_01 = read_input_puzzle_01 (open_in "input_06.txt") in 
  let problems_puzzle_02 = read_input_puzzle_02 (open_in "input_06.txt") in 

  (* List.iter print_problem problems_puzzle_02 ;  *)
  Printf.printf "Puzzle 01 : total = %d\n" (process_problems problems_puzzle_01) ;
  Printf.printf "Puzzle 02 : total = %d\n" (process_problems problems_puzzle_02)

let () = main ()