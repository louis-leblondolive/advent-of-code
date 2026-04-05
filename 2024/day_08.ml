type case = {
  symb : char ; 
  x : int ; 
  y : int ;
  mutable antinode : bool
}

let affiche_grille g = 
  for i = 0 to Array.length g - 1 do 
    for j = 0 to Array.length g - 1 do 
      if g.(i).(j).antinode then 
        Printf.printf "#" 
      else
        Printf.printf "%c" g.(i).(j).symb
    done ; 
    Printf.printf "\n" 
  done

let calcul_antinodes grille c = 
  (* calcule les antinodes qui correspondent à c *)
  let n = Array.length grille in 

  (* 1 : trouver toutes les antennes c *)
  let antennes = ref [] in
  for i = 0 to n - 1 do 
    for j = 0 to n - 1 do 
      if grille.(i).(j).symb = c then 
        antennes := grille.(i).(j) :: !antennes 
    done
  done ; 
  
  (* 2 : pour chaque paire, ajouter les antinodes*)
  let add_antinodes a1 a2 = 
    let dx = a2.x - a1.x in
    let dy = a2.y - a1.y in

    if (a1.x - dx >= 0) && (a1.y - dy >= 0) && (a1.x - dx < n) && (a1.y - dy < n) then begin
      grille.(a1.x - dx).(a1.y - dy).antinode <- true;
    end;

    if (a2.x + dx < n) && (a2.y + dy < n) && (a2.x + dx >= 0) && (a2.y + dy >= 0) then begin
      grille.(a2.x + dx).(a2.y + dy).antinode <- true
    end
  in

  let rec aux_antinodes ant = 
    match ant with 
    | [] -> ()
    | a :: tl -> List.iter (fun a' -> add_antinodes a a') tl ;
                 aux_antinodes tl
  in

  aux_antinodes !antennes

let case_vide c i j = 
  {symb = c; x = i; y = j; antinode = false}

let input_jour08 () = 
  let f = open_in "input08.txt" in 
  let n = 50 in (* n à modifier *)
  let grille = Array.make_matrix n n (case_vide '0' 0 0) in

  let rec aux_input f cpt = 
    match input_line f with 
    | str -> for i = 0 to n - 1 do 
                grille.(cpt).(i) <- case_vide str.[i] cpt i ;
             done ;
             aux_input f (cpt + 1)
    | exception End_of_file -> ()
  in

  aux_input f 0 ;
  grille

let etape1 () = 
  let grille = input_jour08 () in
  let antennes = Hashtbl.create 1000 in
  let n = Array.length grille in 

  for i = 0 to n - 1 do 
    for j = 0 to n - 1 do 
      if grille.(i).(j).symb <> '.' then 
        Hashtbl.replace antennes grille.(i).(j).symb true 
    done 
  done ; 

  Hashtbl.iter (fun c v -> calcul_antinodes grille c) antennes ; 

  let cpt = ref 0 in 
  for i = 0 to n - 1 do 
    for j = 0 to n - 1 do 
      if grille.(i).(j).antinode then begin
        incr cpt 
      end
    done 
  done ; 

  Printf.printf "res1 = %d\n" !cpt
  
(* PARTIE II *)
let calcul_antinodes_evolue grille c = 
  (* calcule les antinodes qui correspondent à c *)
  let n = Array.length grille in 

  (* 1 : trouver toutes les antennes c *)
  let antennes = ref [] in
  for i = 0 to n - 1 do 
    for j = 0 to n - 1 do 
      if grille.(i).(j).symb = c then 
        antennes := grille.(i).(j) :: !antennes 
    done
  done ; 
  
  (* 2 : pour chaque paire, ajouter les antinodes*)
  let add_antinodes_evolue a1 a2 = 

    let rec add_antinodes_aux x y dx dy = 
      
      if (x >= 0) && (y >= 0) 
        && (x < n) && (y < n) then begin
          grille.(x).(y).antinode <- true;
          add_antinodes_aux (x+dx) (y+dy) dx dy 
      end
    in

    let dx = a2.x - a1.x in
    let dy = a2.y - a1.y in

    add_antinodes_aux a1.x a1.y (-dx) (-dy) ;
    add_antinodes_aux a2.x a2.y dx dy 
    (*
    while (a1.x - (!i * dx) >= 0) && (a1.y - (!i * dy) >= 0) 
        && (a1.x - (!i * dx) < n) && (a1.y - (!i * dy) < n) do
      
      grille.(a1.x - (!i*dx)).(a1.y - (!i*dy)).antinode <- true;
      incr i 
    done ; 

    while (a2.x + (!i * dx) < n) && (a2.y + (!i * dy) < n) 
       && (a2.x + (!i * dx) >= 0) && (a2.y + (!i * dy) >= 0) do

      grille.(a2.x + (!i*dx)).(a2.y + (!i*dy)).antinode <- true ; 
      incr i 
    done*)
  in

  let rec aux_antinodes ant = 
    match ant with 
    | [] -> ()
    | a :: tl -> List.iter (fun a' -> add_antinodes_evolue a a') tl ;
                 aux_antinodes tl
  in

  aux_antinodes !antennes


let etape2 () = 
  let grille = input_jour08 () in
  let antennes = Hashtbl.create 1000 in
  let n = Array.length grille in 
  
  for i = 0 to n - 1 do 
    for j = 0 to n - 1 do 
      if grille.(i).(j).symb <> '.' then 

        let s = grille.(i).(j).symb in
        if Hashtbl.mem antennes s then 
          Hashtbl.replace antennes s (Hashtbl.find antennes s + 1)
        else 
          Hashtbl.add antennes s 1
  
    done 
  done ; 
  
  for i = 0 to n - 1 do 
    for j = 0 to n - 1 do 
      if grille.(i).(j).symb <> '.' then 
        if Hashtbl.find antennes grille.(i).(j).symb > 1 then 
          grille.(i).(j).antinode <- true 
    done 
  done ; 

  Hashtbl.iter (fun c v -> calcul_antinodes_evolue grille c) antennes ; 
  
  let cpt = ref 0 in 
  for i = 0 to n - 1 do 
    for j = 0 to n - 1 do 
      if grille.(i).(j).antinode then begin
        incr cpt 
      end
    done 
  done ; 

  Printf.printf "res2 = %d\n" !cpt



let _ = etape2 ()