let rec read_input file = 
  match input_line file with 
  | str -> let l = read_input file in str :: l
  | exception End_of_file -> []

let char_to_int c = 
    Char.code c - Char.code '0'

let max_number s = 
    (* renvoie l'indice du plus grand nombre contenu dans s *)
    (* Printf.printf "searching %s \n" s;  *)
    let n = String.length s in 
    let i_max = ref 0 in 
    for i = 0 to n - 1 do 
      if Char.code s.[i] > Char.code s.[!i_max] then i_max := i 
    done ; 
    !i_max

let puzzle_01 banks = 
  
  let rec total_joltage sub_tot banks = 
    match banks with 
    | [] -> sub_tot
    | bank :: tl -> let n = String.length bank in 
                    let i_max_diz = max_number (String.sub bank 0 (n-1)) in 
                    let i_max_unit = i_max_diz + 1 + max_number (String.sub bank (i_max_diz + 1) (n - i_max_diz - 1)) in 

                    let joltage = (10 * char_to_int bank.[i_max_diz]) + (char_to_int bank.[i_max_unit]) in 

                    (* Printf.printf "bank %s : %d with char %c %c\n" bank joltage bank.[i_max_diz] bank.[i_max_unit] ;  *)

                    total_joltage (sub_tot + joltage) tl
  in

  total_joltage 0 banks


let puzzle_02 banks = 

  let rec total_joltage sub_tot banks = 
    match banks with 
    | [] -> sub_tot
    | bank :: tl -> let n = String.length bank in 
                    let max_digits = Array.make 12 0 in 

                    let pos = ref (-1) in 

                    for i = 0 to 11 do 
                      
                      let max_ind = max_number (String.sub bank (!pos + 1) (n - (!pos + 1) - (11 - i) )) in 
                      max_digits.(i) <- char_to_int bank.[!pos + 1 + max_ind] ; 
                      pos := !pos + 1 + max_ind
                    done ; 

                    let exp = ref 1 in 
                    let joltage = ref 0 in 

                    for i = 11 downto 0 do 
                      joltage := !joltage + max_digits.(i) * !exp ; 
                      exp := 10 * !exp 
                    done ; 

                    Printf.printf "bank %s : %d \n" bank !joltage ;

                    total_joltage (sub_tot + !joltage) tl
  in
  total_joltage 0 banks 
        



let main () = 
  let file = open_in "input_03.txt" in 
  let banks = read_input file in 

  Printf.printf "Puzzle 01 : max joltage = %d\n" (puzzle_01 banks);
  Printf.printf "Puzzle 02 : max joltage = %d\n" (puzzle_02 banks)

let () = main ()