let read_input file = 
  let ranges = String.split_on_char ',' file in 
  List.map (fun rg -> Scanf.sscanf rg "%d-%d" (fun x y -> (x, y))) ranges


let puzzle_01 ranges = 
  
  let is_invalid_id n = 
    let n_str = Int.to_string n in
    let l_str = String.length n_str in

        l_str mod 2 = 0 
    &&  String.sub n_str 0 (l_str / 2) = String.sub n_str (l_str / 2) (l_str / 2)
  in

  let cpt = ref 0 in 
  List.iter (fun (x, y) -> for i = x to y do 
                            if is_invalid_id i then cpt := !cpt + i 
                          done  )
            ranges ;

  !cpt


let puzzle_02 ranges = 

  let dividers_tbl = Hashtbl.create 100 in 

  let regular_substrings s n = 
    (* renvoie une division de s en n sous chaînes de même taille  *)
    (* on doit avoir n | #s *)
    let h = String.length s / n in 
    let res = ref [] in 

    for i = 0 to n - 1 do 
      res := String.sub s (h*i) h :: !res
    done ; 
    !res 
  in

  let dividers n =
    (* renvoie les diviseurs de n sauf 1*)
    if Hashtbl.mem dividers_tbl n then 
      Hashtbl.find dividers_tbl n

    else 
      let div = ref [] in 
      for i = 2 to n do 
         if n mod i = 0 then div := i :: !div
      done ;
      Hashtbl.add dividers_tbl n !div ; 
      !div
  in  

  let rec all l =
    (* true si tous les éléments de l sont égaux *)
    match l with 
    | [] -> true
    | [e] -> true
    | hd1 :: hd2 :: tl -> hd1 = hd2 && all (hd2 :: tl)
  in

  let cpt = ref 0 in 

  List.iter (fun (x, y) -> 
    for n = x to y do 

      let s = Int.to_string n in 

      let verif = List.map (fun k -> all (regular_substrings s k) ) (dividers (String.length s)) in

      if String.length s > 1 && List.exists (fun x -> x = true) verif then begin cpt := !cpt + n ; 
      (*Printf.printf "Invalid ID  : %d \n" n *) end;

    done
            ) 
  ranges ; 

  !cpt 





let main () = 
  let file = input_line (open_in "input_02.txt") in 
  let ranges = read_input file in 

  (*List.iter (fun (x, y) -> Printf.printf "%d-%d\n" x y) ranges*)
  Printf.printf "Puzzle 01 : code = %d\n" (puzzle_01 ranges);
  Printf.printf "Puzzle 02 : code = %d\n" (puzzle_02 ranges)


let () = main ()