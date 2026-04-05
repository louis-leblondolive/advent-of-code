let extrait_min l =
  (* 1 : trouver le min *)
  let m = ref (List.hd l) in
  List.iter (fun i -> if i < !m then m := i) l ;

  (* 2 : retirer le min *)
  let l' = ref [] in
  let rm = ref false in 
  List.iter (fun i -> if not(!rm) && i = !m then rm := true
                      else l' := i :: !l') l ;

  (!m, !l')


let etape1 g d = 

  let rec aux_process dr ga acc =
    match (dr, ga) with 
    | ([], []) -> acc

    | (_ :: tld, _ :: tlg) -> let md, ld = extrait_min dr in
                              let mg, lg = extrait_min ga in 
                              aux_process ld lg (acc + abs(md - mg)) 

    | _ -> failwith "pas de la même taille "
  in

  Printf.printf "res1 : %d \n" (aux_process d g 0)


let etape2 g d = 
  let table_occ = Hashtbl.create 1000 in 
  
  List.iter (fun i -> 
              if Hashtbl.mem table_occ i then 
                  Hashtbl.replace table_occ i (Hashtbl.find table_occ i + 1) 
              else  
                  Hashtbl.add table_occ i 1 
                )  d ; 
              
  let res = ref 0 in
  
  List.iter (fun i -> 
              if Hashtbl.mem table_occ i then 
                res := !res + (i * (Hashtbl.find table_occ i))) g ;

  Printf.printf "res2 : %d \n" !res


let rec read_input file =
  match input_line file with
  | str -> Scanf.sscanf str "%d %d" (fun a b -> let (l1, l2) = read_input file in (a :: l1, b :: l2))
  | exception End_of_file -> ([], [])
  

  
let main () =
  let f = open_in "input01.txt" in
  let (l1, l2) = read_input f in
  
  etape1 l1 l2 ;
  etape2 l1 l2
  
let _ = main ()