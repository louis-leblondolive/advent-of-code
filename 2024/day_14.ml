type rob = {
  x : int ; 
  y : int ;
  v_x : int ; 
  v_y : int
}

let affiche_r r = 
  Printf.printf "x = %d ; y = %d ; v_x = %d ; v_y = %d \n" r.x r.y r.v_x r.v_y

let modulo x y =
  let result = x mod y in
  if result >= 0 then result
  else result + y

let input_jour14 () = 
  let robs = ref [] in 
  let f = open_in "input14.txt" in

  let rec aux_input f = 
    match input_line f with 
    | str -> Scanf.sscanf str "p=%d,%d v=%d,%d" (fun rX rY vrX vrY -> 
                                          robs := {x = rX; y = rY; v_x = vrX; v_y = vrY} :: !robs); 
             aux_input f
    | exception End_of_file -> ()
  in

  aux_input f;
  List.rev !robs


(* PARTIE I *)
let move_rob r moves n_x n_y =
    {x = modulo (r.v_x * moves + r.x) n_x; y = modulo (r.v_y * moves + r.y) n_y; 
    v_x = r.v_x; v_y = r.v_y}


let etape1 () = 
  let robs = input_jour14 () in
  let moves = 100 in 
  let n_x = 101 in 
  let n_y = 103 in 

  let robs_end = List.map (fun r -> move_rob r moves n_x n_y) robs in 

  let q1 = ref 0 in 
  let q2 = ref 0 in 
  let q3 = ref 0 in 
  let q4 = ref 0 in 

  let assign_quadrant r = 
    if r.x < n_x / 2 && r.y < n_y / 2 then 
      incr q1
    else if r.x < n_x / 2 && r.y > n_y / 2 then 
      incr q3
    else if r.x > n_x / 2 && r.y < n_y / 2 then 
      incr q2 
    else if r.x > n_x / 2 && r.y > n_y / 2 then 
      incr q4
  in

  List.iter assign_quadrant robs_end ;
  Printf.printf "res1 = %d\n" (!q1 * !q2 * !q3 * !q4)

let _ = etape1 ()

(* PARTIE II *)
let pause_ms ms =
  let start_time = Sys.time () in
  let target_time = start_time +. (float_of_int ms /. 1000.0) in
  while Sys.time () < target_time do
    ()  (* Ne rien faire, juste attendre *)
  done

let etape2 () = 
  let robs = input_jour14 () in 
  let n_x = 101 in 
  let n_y = 103 in 
  let cpt = ref 2553 in 

  while true do 
    Printf.printf "elapsed : %d\n" !cpt; 
    let grille = Array.make_matrix n_x n_y 0 in
    
    let current_robs = List.map (fun r -> move_rob r !cpt n_x n_y) robs in 
    List.iter (fun r -> grille.(r.x).(r.y) <- grille.(r.x).(r.y) + 1) current_robs ;

    for j = 0 to n_y - 1 do 
      if grille.(0).(j) = 0 then 
        Printf.printf " "
      else 
        Printf.printf "%d" grille.(0).(j) 
    done ; 
    print_newline ();

    for i = 1 to 50 do 
      for j = 0 to n_y - 1 do 
        if grille.(i).(j) = 0 then 
          Printf.printf " "
        else 
          Printf.printf "%d" grille.(i).(j) 
      done ; 

      (*Printf.printf "|";*)

    for j = 0 to n_y - 1 do 
      if grille.(i+50).(j) = 0 then 
          Printf.printf " "
        else 
          Printf.printf "%d" grille.(i+50).(j) 
    done ; 

    print_newline ()
    done ; 
    pause_ms 500 ;
    incr cpt
  done
     

let _ = etape2 ()