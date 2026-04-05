type case = {
  mutable c : char ; 
  mutable nord : bool ;
  mutable sud : bool ;
  mutable est : bool ;
  mutable ouest : bool 
}
let case_vide elt = 
  {c = elt; nord = false; sud = false; est = false; ouest = false}

let affiche_tab tab = 
  for i = 0 to Array.length tab - 1 do 
    for j = 0 to Array.length tab - 1 do 
      Printf.printf "%c" tab.(i).(j).c
    done; 
    Printf.printf "\n" 
  done ; 
  Printf.printf "\n"

let virage90 dx dy = 
  if dx = 0 && dy = 1 then 
    (1, 0)
  else if dx = 1 && dy = 0 then 
    (0, -1)
  else if dx = 0 && dy = -1 then 
    (-1, 0)
  else  
    (0, 1)


let nettoie_grille grille = 
  for i = 0 to Array.length grille - 1 do 
    for j = 0 to Array.length grille - 1 do 
      
     grille.(i).(j) <- case_vide (grille.(i).(j).c)

    done
  done


let marque_visite grille x y dx dy =
  let case = grille.(x).(y) in
  if dy = 1 then 
    case.est <- true
  else if dx = 1 then 
    case.sud <- true
  else if dy = -1 then 
    case.ouest <- true
  else (*dx = -1*)
    case.nord <- true 


let rec detecte_boucle grille x y dx dy = 

  (*Printf.printf "pos : %d %d, dir : %d %d \n" x y dx dy ;*)

  if x + dx >= Array.length grille || y + dy >= Array.length grille || 
      x + dx < 0 || y + dy < 0 then begin
        (*Printf.printf "sortie en %d %d \n" (x+dx) (y+dy) ;*)
        false
  end

  else begin
      let case = grille.(x).(y) in 

      if (case.nord && dx = -1) || (case.sud && dx = 1) 
        || (case.est && dy = 1) || (case.ouest && dy = -1) then begin
        Printf.printf "loop detected %d %d \n" x y ;
        true
      end
      
    else begin

      marque_visite grille x y dx dy ;

      if grille.(x + dx).(y + dy).c = '#' then begin

        let ddx, ddy = virage90 dx dy in 
        (*marque_visite grille x y ddx ddy ; 
        marque_visite grille x y dx dy ; *)
        detecte_boucle grille x y ddx ddy
        
      end
      
      else begin
        detecte_boucle grille (x + dx) (y + dy) dx dy
      end

    end
  end
  



let rec parcours_garde grille x y dx dy = 

  grille.(x).(y) <- 'O' ; 

  if x + dx >= Array.length grille || y + dy >= Array.length grille || 
    x + dx < 0 || y + dy < 0 then 
      ()

  else begin
    if grille.(x + dx).(y + dy) = '#' then begin
      let ddx, ddy = virage90 dx dy in 
      parcours_garde grille x y ddx ddy 
      end
    else begin
      parcours_garde grille (x + dx) (y + dy) dx dy 
    end
  end


let etape1 () = 
  let f = open_in "input06.txt" in

  let n = 130 in 
  let grille = Array.make_matrix n n '.' in  

  let rec aux_input f cpt = 
    match input_line f with 
    | str -> for i = 0 to n - 1 do 
                grille.(cpt).(i) <- str.[i] 
            done ;
            aux_input f (cpt + 1)
    | exception End_of_file -> ()
  in

  aux_input f 0 ;
  let x_dep = ref 0 in 
  let y_dep = ref 0 in 

  for i = 0 to n - 1 do 
    for j = 0 to n - 1 do 
      if grille.(i).(j) = '^' then begin
        grille.(i).(j) <- '.' ; 
        x_dep := i ; 
        y_dep := j
      end
    done
  done ; 

  Printf.printf "%d %d \n" !x_dep !y_dep; 
  parcours_garde grille !x_dep !y_dep (-1) 0 ;

  let cpt = ref 0 in 
  for i = 0 to n - 1 do 
    for j = 0 to n - 1 do 
      if grille.(i).(j) = 'O' then 
          incr cpt
    done
  done ; 
  Printf.printf "%d\n" !cpt


let etape2 () = 
  let f = open_in "input06.txt" in

  let n = 130 in 
  let grille = Array.make_matrix n n (case_vide '.') in  

  let rec aux_input f cpt = 
    match input_line f with 
    | str -> for i = 0 to n - 1 do 
                grille.(cpt).(i) <- case_vide str.[i]
            done ;
            aux_input f (cpt + 1)
    | exception End_of_file -> ()
  in

  aux_input f 0 ;

  let x_dep = ref 0 in 
  let y_dep = ref 0 in 

  for i = 0 to n - 1 do 
    for j = 0 to n - 1 do 
      if grille.(i).(j).c = '^' then begin
        grille.(i).(j).c <- '.' ; 
        x_dep := i ; 
        y_dep := j
      end
    done
  done ; 

  Printf.printf "départ : %d %d \n" !x_dep !y_dep;

  
  let cpt = ref 0 in 

  for i = 0 to n - 1 do 
    for j = 0 to n - 1 do 

      if (i <> !x_dep || j <> !y_dep) && grille.(i).(j).c <> '#' then begin 

        grille.(i).(j) <- case_vide '#'; 

        if (detecte_boucle grille !x_dep !y_dep (-1) 0) then begin
          incr cpt 
        end ;

        grille.(i).(j) <- case_vide '.' ; 
        
      end ;
      nettoie_grille grille
    done
   
  done ; 
  Printf.printf "res2 = %d\n" !cpt 
  (*Printf.printf "boucle : %b\n" (detecte_boucle grille !x_dep !y_dep (-1) 0)*)


let _ = etape2 ()