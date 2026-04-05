type case = {
  symb : char ; 
  mutable vue : bool
}

let affiche_grille g = 
  let n = Array.length g in 
  for i = 0 to n - 1 do 
    for j = 0 to n - 1 do 
      Printf.printf "%c" g.(i).(j).symb
    done;
    Printf.printf "\n"
  done ; 
  Printf.printf "\n" 

let case_vide c = 
  {symb = c; vue = false}

let input_jour12 () = 
  let f = open_in "input12.txt" in
  let n = 140 in                     (* n à modifier *)
  let grille = Array.make_matrix n n (case_vide '0') in

  let rec aux_input cpt = 
    match input_line f with 
    | str -> String.iteri (fun i elt -> grille.(cpt).(i) <- case_vide elt) str ;
            aux_input (cpt + 1)
    | exception End_of_file -> ()
  in

  aux_input 0 ;
  (*
  for i = 0 to n - 1 do 
    for j = 0 to n - 1 do
      Printf.printf "%c " grille.(i).(j).symb 
    done; 
    Printf.printf "\n" ;
  done;
   Printf.printf "\n";*)
  grille

(* PARTIE I *)
let rec explore_region grille plante per aire x y =
  let n = Array.length grille in 
  
  if x >= n || y >= n || x < 0 || y < 0 then  (* on dépasse -> on rajoute 1 barrière *)
    incr per 

  else begin 
    if grille.(x).(y).symb <> plante then (* sortie région *)
      incr per 
    else begin 
      if grille.(x).(y).vue then  (* dans région, déjà visitée *)
        ()
      else begin                  (* on est sur une bonne case -> on explore tout autour *)
        incr aire;
        grille.(x).(y).vue <- true;
        explore_region grille plante per aire (x + 1) y ;
        explore_region grille plante per aire (x - 1) y ;
        explore_region grille plante per aire x (y + 1) ;
        explore_region grille plante per aire x (y - 1) 
      end
    end
  end


let etape1 () = 
  let grille = input_jour12 () in
  let n = Array.length grille in 

  let cpt =  ref 0 in 

  for x = 0 to n - 1 do 
    for y = 0 to n - 1 do 
      if not grille.(x).(y).vue then begin 
        let per = ref 0 in 
        let aire = ref 0 in 
        explore_region grille grille.(x).(y).symb per aire x y ;
        cpt := !cpt + !per * !aire ; 
        (*Printf.printf "%c : %d %d \n" grille.(x).(y).symb !per !aire*)
      end 
    done
  done ; 

  Printf.printf "res1 = %d\n" !cpt

let _ = etape1 ()

(* PARTIE II *)
let aire grille plante x y = 
  let n = Array.length grille in
  let a = ref 0 in 

  let rec aux_explore x y = 
    if x >= n || y >= n || x < 0 || y < 0 
      || grille.(x).(y).symb <> plante || grille.(x).(y).vue then 
        ()

    else begin 
      incr a;
      grille.(x).(y).vue <- true;
      aux_explore (x + 1) y;
      aux_explore (x - 1) y;
      aux_explore x (y + 1);
      aux_explore x (y - 1)
    end
  in

  aux_explore x y ;
  !a

let copie grille plante x y = 
  (* grille avec . sauf région de x y *)
  let n = Array.length grille in
  let grille_c = Array.make_matrix n n (case_vide '.') in

  let rec aux_explore x y = 
    if x >= n || y >= n || x < 0 || y < 0 
      || grille_c.(x).(y).symb <> '.' || grille.(x).(y).symb <> plante then 
        ()

    else begin 
      grille_c.(x).(y) <- case_vide plante ; 
      aux_explore (x + 1) y;
      aux_explore (x - 1) y;
      aux_explore x (y + 1);
      aux_explore x (y - 1)
    end
  in

  aux_explore x y ;
  grille_c


let nb_cotes_ajoutes grille x y =
  (* renvoie combien de côtés on ajoute quand on place un carré en x y *)
  
  if grille.(x).(y).symb = '.' then 0
  else begin 
  
  (* les tests en question :'( *)
  if grille.(x).(y-1).symb = '.' && grille.(x-1).(y-1).symb = '.' && grille.(x-1).(y).symb = '.' then 
    4 (*1*)
  else if grille.(x).(y-1).symb = '.' && grille.(x-1).(y-1).symb <> '.' && grille.(x-1).(y).symb = '.' then 
    4 (*2*)
  else if grille.(x).(y-1).symb = '.' && grille.(x-1).(y-1).symb = '.' && grille.(x-1).(y).symb <> '.' then 
    0 (*3*)
  else if grille.(x).(y-1).symb <> '.' && grille.(x-1).(y-1).symb = '.' && grille.(x-1).(y).symb = '.' 
    && grille.(x+1).(y-1).symb = '.' then 
    0 (*4a*)

  else if grille.(x).(y-1).symb <> '.' && grille.(x-1).(y-1).symb = '.' && grille.(x-1).(y).symb = '.' 
      && grille.(x+1).(y-1).symb <> '.' then 
    2 (*4b*)

  else if grille.(x).(y-1).symb = '.' && grille.(x-1).(y-1).symb <> '.' && grille.(x-1).(y).symb <> '.' then 
    2 (*5*)

  else if grille.(x).(y-1).symb <> '.' && grille.(x-1).(y-1).symb <> '.' && grille.(x-1).(y).symb = '.'
    && grille.(x+1).(y-1).symb = '.' then 
    2 (*6a*)
  else if grille.(x).(y-1).symb <> '.' && grille.(x-1).(y-1).symb <> '.' && grille.(x-1).(y).symb = '.'
    && grille.(x+1).(y-1).symb <> '.' then 
    4 (*6b*)

  else if grille.(x).(y-1).symb <> '.' && grille.(x-1).(y-1).symb = '.' && grille.(x-1).(y).symb <> '.'
    && grille.(x+1).(y-1).symb = '.' then 
    -2 (*7a*)
else if grille.(x).(y-1).symb <> '.' && grille.(x-1).(y-1).symb = '.' && grille.(x-1).(y).symb <> '.'
  && grille.(x+1).(y-1).symb <> '.' then 
  0 (*7b*)


  else if grille.(x).(y-1).symb <> '.' && grille.(x-1).(y-1).symb <> '.' && grille.(x-1).(y).symb <> '.'
    && grille.(x+1).(y-1).symb = '.' then 
    -2 (*8a*)
else if grille.(x).(y-1).symb <> '.' && grille.(x-1).(y-1).symb <> '.' && grille.(x-1).(y).symb <> '.'
  && grille.(x+1).(y-1).symb <> '.' then 
  0 (*8b*)

  else begin 
    Printf.printf "cas bien douteux \n" ;
    0 
  end
  end


let nb_cotes_region grille_p x y = 

  let reg = grille_p.(x).(y).symb in 
  let n = Array.length grille_p in 

  let grille' = copie grille_p reg x y in
  let grille = Array.make_matrix (n+2) (n+2) (case_vide '.') in

  (* élargissement pour simplifier les cas de test *)
  for i = 1 to n  do 
    for j = 1 to n  do 
      grille.(i).(j) <- case_vide grille'.(i-1).(j-1).symb
    done
  done ; 

  let cotes = ref 0 in 
    for j = 0 to n - 1 do 
      for i = 0 to n - 1 do 
        let n = nb_cotes_ajoutes grille (i+1) (j+1)  in
        (*Printf.printf "x:%d y:%d n:%d\n" i j n ;*)
        cotes := !cotes + n
      done
    done ;
  !cotes
  

let etape2 () = 
  let grille = input_jour12 () in
  let n = Array.length grille in 
  (*Printf.printf "%d\n" (nb_cotes_region grille 1 3)*)
  let total = ref 0 in 
  
  for x = 0 to n - 1 do 
    for y = 0 to n - 1 do
      if not grille.(x).(y).vue then 
        total := !total + (aire grille grille.(x).(y).symb x y) * (nb_cotes_region grille x y)
    done 
  done ; 
  Printf.printf "%d\n" !total

let _ = etape2 ()