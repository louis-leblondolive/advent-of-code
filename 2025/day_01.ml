let rec read_input file = 
  match input_line file with 
  | str -> Scanf.sscanf str "%c%d" (fun c n -> let l = read_input file in (c, n) :: l) 
  | exception End_of_file -> []


let modulo a b = 
  (* renvoie a modulo b *)
  if a >= 0 then 
    a mod b 
  else
    let a' = ref a in 
    while !a' < 0 do 
      a' := !a' + b
    done ; 
    !a' mod b


let puzzle_01 instructions = 

  let cpt = ref 0 in 

  let code = List.fold_left (fun code (instr, n) ->  
                                      if code = 0 then incr cpt ; 
                                      if instr = 'R' then 
                                            modulo (code + n) 100
                                       else modulo (code - n) 100 )

                50 instructions 
      in

  if code = 0 then incr cpt ; 
  !cpt


let puzzle_02 instructions = 

  let cpt = ref 0 in 
  let dial = ref 50 in 

  List.iter (fun (c, n) -> let n' = ref n in 
                           while !n' > 0 do 
                              n' := !n' - 1 ;
                              if c = 'R' then 
                                dial := modulo (!dial + 1) 100
                              else dial := modulo (!dial - 1) 100 ;
                              if !dial = 0 then incr cpt 
                            done                          )
            instructions ;

  !cpt


let main () = 
  let file = open_in "input_01.txt" in 
  let instructions = read_input file in 

  (*List.iter (fun (c, n) -> Printf.printf "%c%d\n" c n) instructions ;*)

  Printf.printf "Puzzle 1 : code = %d\n" (puzzle_01 instructions) ;
  Printf.printf "Puzzle 2 : code = %d\n" (puzzle_02 instructions)

let () = main ()