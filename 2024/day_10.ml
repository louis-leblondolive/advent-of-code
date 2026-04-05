type case = {
  symb : int ;
  mutable vue : bool
}

let case_vide s =
  {symb = s ; vue = false}

let input_jour10 () = 
  let f = open_in "input10.txt" in
  let n = 53 in                      (* n à modifier *)
  let grille = Array.make_matrix n n (case_vide (-1)) in

  let rec aux_input cpt =
    match input_line f with 
    | str -> String.iteri (fun i c -> grille.(cpt).(i) <- case_vide (int_of_char c - 48)) str ;
             aux_input (cpt + 1)

    | exception End_of_file -> ()
  in

  aux_input 0 ;
  grille
  
(* PARTIE I *)
  let score grille i j = 
    let n = Array.length grille in 
  
    let rec randonnee x y prev_s = 
    
      if x >= n || y >= n || x < 0 || y < 0 
                || prev_s + 1 <> grille.(x).(y).symb then 
        ()
    
      else begin 
        let s = grille.(x).(y).symb in
        if s = 9 then begin 
          grille.(x).(y).vue <- true 
        end
        else begin 
          randonnee  (x+1) y s;
          randonnee  (x-1) y s;
          randonnee  x (y+1) s;
          randonnee  x (y-1) s
        end
      end
    in
  
    randonnee  (i+1) j 0;
    randonnee  (i-1) j 0;
    randonnee  i (j+1) 0;
    randonnee  i (j-1) 0;
  
    let score = ref 0 in 
  
    for x = 0 to n - 1 do 
      for y = 0 to n - 1 do 
        if grille.(x).(y).vue then begin 
          incr score 
        end ; 
        grille.(x).(y).vue <- false
      done
    done ; 
  
    !score
        
  let etape1 () = 
    let grille = input_jour10 () in
    let n = Array.length grille in 
  
    let cpt = ref 0 in 
  
    for x = 0 to n - 1 do 
      for y = 0 to n - 1 do 
        if grille.(x).(y).symb = 0 then begin
          cpt := !cpt + (score grille x y)
        end
      done ; 
    done ;
  
    Printf.printf "res1 = %d \n" !cpt
  
  let _ = etape1 ()


(* PARTIE II *)
let rec randonnee grille x y prev_s cpt = 
  let n = Array.length grille in 

  if x >= n || y >= n || x < 0 || y < 0 
            || prev_s + 1 <> grille.(x).(y).symb then 
    ()

  else begin 
    let s = grille.(x).(y).symb in
    if s = 9 then begin 
      grille.(x).(y).vue <- true ;
      incr cpt
    end
    else begin 
      randonnee grille (x+1) y s cpt;
      randonnee grille (x-1) y s cpt;
      randonnee grille x (y+1) s cpt;
      randonnee grille x (y-1) s cpt
    end
  end

let etape2 () = 
  let grille = input_jour10 () in
  let n = Array.length grille in 

  let cpt = ref 0 in 

  for x = 0 to n - 1 do 
    for y = 0 to n - 1 do 
      if grille.(x).(y).symb = 0 then begin
        let a = ref 0 in 
        randonnee grille (x+1) y 0 a;
        randonnee grille (x-1) y 0 a;
        randonnee grille x (y+1) 0 a;
        randonnee grille x (y-1) 0 a;
        cpt := !cpt + !a
      end
    done ; 
  done ;

  Printf.printf "res2 = %d \n" !cpt


let _ = etape2 ()