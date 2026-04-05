let rec recherche_dir mat mot cpt etat x y dx dy = 
    if x >= Array.length mat || y >= Array.length mat
      || x < 0 || y < 0 then
      if etat >= Array.length mot then 
        cpt + 1 
      else 
        cpt
    (*else begin
      mat.(x).(y) <- 1 + mat.(x).(y) ;
      recherche_dir mat mot 0 etat (x+dx) (y+dy) dx dy end*)
    
    else begin
      if etat >= Array.length mot then begin
        Printf.printf "at %d %d \n" x y ;
        recherche_dir mat mot (cpt + 1) 0 x y dx dy 
      end

      else begin
        if mot.(etat) = mat.(x).(y) then 
          recherche_dir mat mot cpt (etat + 1) (x + dx) (y + dy) dx dy

        else begin
          if mat.(x).(y) = mot.(0) then 
            recherche_dir mat mot cpt 1 (x + dx) (y + dy) dx dy
          else 
            recherche_dir mat mot cpt 0 (x + dx) (y + dy) dx dy
        end
      end
    end 



let affiche_tab mat = 
  let n = Array.length mat in 
  for i = 0 to n - 1 do 
    for j = 0 to n - 1 do 
      Printf.printf "%d " mat.(i).(j) 
    done ; 
    Printf.printf "\n"
  done


let etape1 mat = 
  let n = Array.length mat in 
  let cpt = ref 0 in 
  let mot = [|'X'; 'M'; 'A'; 'S'|] in

  for x = 0 to n - 1 do
    (* Parcours des lignes *)
    cpt := !cpt + (recherche_dir mat mot 0 0   x   0     0   1  ) ; 
    cpt := !cpt + (recherche_dir mat mot 0 0   x (n-1)   0 (-1))  
  done;

  for y = 0 to n - 1 do 
    (* Parcours des colonnes *)
    cpt := !cpt + (recherche_dir mat mot 0 0    0   y      1  0) ; 
    cpt := !cpt + (recherche_dir mat mot 0 0  (n-1) y    (-1) 0) 
  done ;

  (* Parcours des diagonales *)

  for z = 0 to n - 1 do 
    cpt := !cpt + (recherche_dir mat mot 0 0   z    0        1    1  ) ; 
    cpt := !cpt + (recherche_dir mat mot 0 0   0  (z+1)      1    1  ) ;
  done ; 
  
  for z = 0 to n - 1 do 
    cpt := !cpt + (recherche_dir mat mot 0 0   (z+1) (n-1)    1  (-1)  ) ; 
    cpt := !cpt + (recherche_dir mat mot 0 0     0     z      1  (-1)  ) ;
  done ; 
  
  for z = 0 to n - 1 do 
    cpt := !cpt + (recherche_dir mat mot 0 0   z       0        (-1) 1  ) ; 
    cpt := !cpt + (recherche_dir mat mot 0 0  (n-1)  (z+1)      (-1) 1  ) ;
  done ; 

  for z = 0 to n - 1 do 
    cpt := !cpt + (recherche_dir mat mot 0 0   (n-2-z) (n-1)    (-1)  (-1)  ) ; 
    cpt := !cpt + (recherche_dir mat mot 0 0   (n-1)   z      (-1)  (-1)  ) 
  done ;


  Printf.printf "%d\n" !cpt ;
  Printf.printf "res1 = %d \n" !cpt


let etape2 mat = 
  let n = Array.length mat in 
  let cpt = ref 0 in 

  for i = 0 to n - 3 do 
    for j = 0 to n - 3 do 

      if mat.(i).(j) = 'M' && mat.(i+1).(j+1) = 'A' && mat.(i+2).(j+2) = 'S' (* M M *)
        && mat.(i+2).(j) = 'S' && mat.(i).(j+2) = 'M' then                   (*  A *)
        incr cpt   ;                                                          (* S S*)

      if mat.(i).(j) = 'M' && mat.(i+1).(j+1) = 'A' && mat.(i+2).(j+2) = 'S' (* M S *)
        && mat.(i+2).(j) = 'M' && mat.(i).(j+2) = 'S' then                   (*  A *)
        incr cpt   ;                                                          (* M S*)

        if mat.(i).(j) = 'S' && mat.(i+1).(j+1) = 'A' && mat.(i+2).(j+2) = 'M' (* S M *)
          && mat.(i+2).(j) = 'S' && mat.(i).(j+2) = 'M' then                   (*  A *)
          incr cpt   ;                                                          (* S M*)
  
        if mat.(i).(j) = 'S' && mat.(i+1).(j+1) = 'A' && mat.(i+2).(j+2) = 'M' (* S S *)
          && mat.(i+2).(j) = 'M' && mat.(i).(j+2) = 'S' then                   (*  A *)
          incr cpt                                                             (* M M*)
    done
  done; 
  Printf.printf "res2 = %d\n" !cpt


let main () = 
  let f = open_in "input04.txt" in

  let n = 140 in 
  
  let mat = Array.make_matrix n n '0' in 
  
  for i = 0 to n - 1 do 
    let li = input_line f in
    for j = 0 to n - 1 do 
      mat.(i).(j) <- li.[j] 
    done 
  done ;
  etape1 mat ;
  etape2 mat

let _ = main ()