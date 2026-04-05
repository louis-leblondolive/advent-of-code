let input_table f = 
  let table = Hashtbl.create 100 in 
  for i = 0 to 99 do 
    Hashtbl.add table i []
  done ;

  let rec aux_cons f = 

    try

      let str = input_line f in
      let l = List.map int_of_string (String.split_on_char '|' str) in 
      let v = List.hd l in 
      let k = List.hd (List.tl l) in 

      Hashtbl.replace table k (v :: Hashtbl.find table k) ;
      aux_cons f

    with
    | End_of_file -> ()

  in
  aux_cons f ; 
  table

exception Invalide 
let est_valide table rep = 

  let rec aux rep = 
    match rep with 
    | [] -> ()
    | hd :: tl -> let verif = Hashtbl.find table hd in 
                  List.iter (fun elt -> if (List.mem elt tl) then raise Invalide) verif ;
                  aux tl 
  in

  try 
    aux rep;
    true
  with 
  | Invalide -> false

let mediane l = 
  let n = List.length l in 

  let rec n_ieme l i = 
    match l with 
    | [] -> failwith "pas assez d'elts"
    | hd :: tl -> if i = (n/2) then hd
                  else n_ieme tl (i+1) 
  in

  n_ieme l 0   
    


let etape1 () = 
  
  let f = open_in "input05_2.txt" in
  let lis = ref [] in

  let rec aux_input f = 
    match input_line f with 
    | str -> lis := List.map int_of_string (String.split_on_char ',' str) :: !lis ;
             aux_input f
    | exception End_of_file -> ()
  in

  aux_input f ;
  
  let table = input_table (open_in "input05_1.txt") in

  let cpt = ref 0 in 
  List.iter (fun elt -> if est_valide table elt then begin
                             cpt := !cpt + (mediane elt) ; 
                             Printf.printf "valide \n"
                        end) !lis ;
  Printf.printf "%d\n" !cpt


let etape2 () = 
  let f = open_in "input05_2.txt" in
  let lis = ref [] in

  let rec aux_input f = 
    match input_line f with 
    | str -> lis := List.map int_of_string (String.split_on_char ',' str) :: !lis ;
             aux_input f
    | exception End_of_file -> ()
  in

  aux_input f ;
  
  let table = input_table (open_in "input05_1.txt") in

  let n = 1000 in

  let aux_repare rep i = 
    if i >= Array.length rep then 
      ()
    else
      let requis = Hashtbl.find table rep.(i) in
      
      for j = i + 1 to (Array.length rep - 1) do 
        if List.mem rep.(j) requis then 
          let cache = rep.(j) in
          rep.(j) <- rep.(i) ; 
          rep.(i) <- cache
      done 
  in

  let cpt = ref 0 in 

  List.iter (fun rep -> if not (est_valide table rep) then 

                          let rep' = Array.of_list rep in 
                          while not (est_valide table (Array.to_list rep')) do 
                            for i = 0 to Array.length rep' do 
                              aux_repare rep' i 
                            done 
                          done ; 
                          cpt := !cpt + (mediane (Array.to_list rep'))
    ) !lis ;

  Printf.printf "res2 = %d \n" !cpt 

  


  
let () = 
  etape1 () ;
  etape2 ()


(*
let input_graphe f = 
  let g = Array.make 100 [] in

  let rec aux_cons f = 
    let str = input_line f in
    if str = "\n" then 
        ()
    else begin 
        let l = List.map int_of_string (String.split_on_char '|' str) in 
        let source = List.hd l in 
        let dest = List.hd (List.tl l) in 
        g.(source) <- dest :: g.(source)
    end 
  in

  aux_cons f ; 
  g


exception Trouve
let chemin_existe g a b = 
  let n = Array.length g in 
  let visites = Array.make n false in 

  let rec aux_dfs s = 
    if not visites.(s) then begin 
      visites.(s) <- true ;
      if s = a then raise Trouve  ;
      List.iter aux_dfs g.(s) 
      end
  in

  try
    aux_dfs a ;
    false
  with 
  | Trouve -> true


exception Invalide
let est_tri_topologique g tri = 
  let tri_inv = List.rev tri in 

  let rec aux l = 
    match l with 
    | [] -> ()
    | hd :: tl -> List.iter (fun i -> if not (chemin_existe g hd i) then raise Invalide) tl ; 
                  aux tl 
  in

*)


(*
let input_tri_topo f = 

  let order = Hashtbl.create 1000 in 

  let rec aux_cons f = 
    match input_line f with 
    | str -> if str = "\n" then 
              ()
             else 
              let l = String.split_on_char '|' str in 
              
              let k = List.hd l in 
              let v = List.hd (List.tl l) in 
              

    | _ -> ()*)