let rec read_input file = 
  match input_line file with 
  | str -> let l = read_input file in str :: l 
  | exception End_of_file -> []


let puzzle_01 grid = 

  let dirs = [|
    (-1, -1) ; (-1, 0) ; (-1, 1) ;
    (0, -1)  ;           (0, 1) ;
    (1, -1)  ; (1, 0)  ; (1, 1) |] in 

  let cpt = ref 0 in 
  let n = Array.length grid in 

  for x = 0 to n - 1 do 
    for y = 0 to n - 1 do 
    
    if grid.(x).(y) = '@' then begin 

      let adj_rolls = Array.fold_left (fun rolls (dx, dy) -> 
          if 0 <= x + dx && x + dx < n && 0 <= y + dy && y + dy < n && grid.(x+dx).(y+dy) = '@' then 
            rolls + 1
          else
            rolls 
      ) 0 dirs 
      in
      if adj_rolls < 4 then incr cpt

    end

    done 
  done ; 
  !cpt


let puzzle_02 grid = 

  let removables_rolls grid = 
    (* renvoie la liste des rouleaux à supprimer *)
    let dirs = [|
      (-1, -1) ; (-1, 0) ; (-1, 1) ;
      (0, -1)  ;           (0, 1) ;
      (1, -1)  ; (1, 0)  ; (1, 1) |] in 

    let removables = ref [] in 
    let n = Array.length grid in 

    for x = 0 to n - 1 do 
      for y = 0 to n - 1 do 
    
      if grid.(x).(y) = '@' then begin 

        let adj_rolls = Array.fold_left (fun rolls (dx, dy) -> 
            if 0 <= x + dx && x + dx < n && 0 <= y + dy && y + dy < n && grid.(x+dx).(y+dy) = '@' then 
              rolls + 1
            else
              rolls 
        ) 0 dirs 
        in
        if adj_rolls < 4 then removables := (x, y) :: !removables

      end

      done 
    done ; 
    !removables
  in

  let cpt = ref 0 in 
  while not (List.is_empty (removables_rolls grid)) do 

    List.iter (fun (x, y) -> grid.(x).(y) <- '.' ; incr cpt) (removables_rolls grid)
  done ; 

  !cpt


let main () = 
  (* init grille *)
  let file = open_in "input_04.txt" in 
  let lines = read_input file in 
  
  let grid = Array.make (List.length lines) [||] in
  let n = String.length (List.hd lines) in 
  
  List.iteri (fun i line -> grid.(i) <- Array.make n '.' ; 
                            String.iteri (fun pos c -> grid.(i).(pos) <- c) line
  ) lines ;

  (* execution *)
  Printf.printf "Puzzle 01 : %d accessible rolls \n" (puzzle_01 grid);
  Printf.printf "Puzzle 02 : %d rolls removed \n" (puzzle_02 grid)


let () = main ()