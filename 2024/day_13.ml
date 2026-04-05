type machine = {
  mutable a_x : int ; 
  mutable a_y : int ; 
  mutable b_x : int ;
  mutable b_y : int ; 
  mutable prize_x : int ;
  mutable prize_y : int 
}


let machine_vide () = 
  {a_x = 0; a_y = 0; b_x = 0; b_y = 0; prize_x = 0; prize_y = 0}

let affiche_m m = 
  Printf.printf "aX : %d aY : %d ; bX : %d bY : %d ;  pX : %d pY : %d \n" 
  m.a_x m.a_y m.b_x m.b_y m.prize_x m.prize_y

let input_jour13_1 () = 
  let f = open_in "input13.txt" in
  let machines = ref [] in

  let rec aux_read f = 
    try 
      let str0 = input_line f in
      let str1 = input_line f in
      let str2 = input_line f in
      if input_line f = " " then (* sert à sauter de paragraphe *)
        Printf.printf "blah" ;

      let m = machine_vide () in

      Scanf.sscanf str0 "Button A: X+%d, Y+%d" (fun xA yA -> m.a_x <- xA ; m.a_y <- yA) ;
      Scanf.sscanf str1 "Button B: X+%d, Y+%d" (fun xB yB -> m.b_x <- xB ; m.b_y <- yB) ;
      Scanf.sscanf str2 "Prize: X=%d, Y=%d" (fun xP yP -> m.prize_x <- xP ; m.prize_y <- yP) ;
      
      machines := m :: !machines;

      aux_read f

    with
    | End_of_file -> ()
    | Scanf.Scan_failure exn -> Printf.printf "erreur dans le scan" 
  in

  aux_read f ;
  List.rev !machines


(* PARTIE I *)
let nb_jetons m = 
  let detA = m.a_x * m.b_y -m.b_x * m.a_y in 
  if detA <> 0 then begin
    let x = ( m.b_y * m.prize_x - m.prize_y * m.b_x ) / detA in
    let y = ( m.a_x * m.prize_y - m.prize_x * m.a_y ) / detA in 
    if x < 100 && y < 100 && (m.a_x * x + m.b_x * y) = m.prize_x && (m.a_y * x + m.b_y * y) = m.prize_y then begin
      (*Printf.printf "x = %d ; y = %d \n" x y ; *)
      (3*x + y)
    end
    else 0
  end
  else 
    0

let etape1 () = 
  let machines = input_jour13_1 () in
  (*
  Printf.printf "nb machines = %d\n" (List.length machines);
  List.iter affiche_m machines ;*)
  let total = ref 0 in 

  List.iter (fun m -> (*Printf.printf "%d\n" (nb_jetons m)*) total := !total + (nb_jetons m)) machines ; 
  Printf.printf "res1 = %d\n" !total

let _ = etape1 ()


(* PARTIE II *)
let input_jour13_2 () = 
  let f = open_in "input13.txt" in
  let machines = ref [] in

  let rec aux_read f = 
    try 
      let str0 = input_line f in
      let str1 = input_line f in
      let str2 = input_line f in
      if input_line f = " " then (* sert à sauter de paragraphe *)
        Printf.printf "blah" ;

      let m = machine_vide () in

      let convert_prize_coordinate p =
        p + 10000000000000
      in

      Scanf.sscanf str0 "Button A: X+%d, Y+%d" (fun xA yA -> m.a_x <- xA ; m.a_y <- yA) ;
      Scanf.sscanf str1 "Button B: X+%d, Y+%d" (fun xB yB -> m.b_x <- xB ; m.b_y <- yB) ;
      Scanf.sscanf str2 "Prize: X=%d, Y=%d" (fun xP yP -> m.prize_x <- convert_prize_coordinate xP ; 
                                                          m.prize_y <- convert_prize_coordinate yP);
      
      machines := m :: !machines;

      aux_read f

    with
    | End_of_file -> ()
    | Scanf.Scan_failure exn -> Printf.printf "erreur dans le scan" 
  in

  aux_read f ;
  List.rev !machines

let nb_jetons_evolue m = 
  let detA = m.a_x * m.b_y -m.b_x * m.a_y in 
  if detA <> 0 then begin
    let x = ( m.b_y * m.prize_x - m.prize_y * m.b_x ) / detA in
    let y = ( m.a_x * m.prize_y - m.prize_x * m.a_y ) / detA in 
    (*if x < 100 && y < 100 && (m.a_x * x + m.b_x * y) = m.prize_x && (m.a_y * x + m.b_y * y) = m.prize_y then begin*)
      Printf.printf "x = %d ; y = %d ; verif : %b \n" x y ((m.a_x * x + m.b_x * y) = m.prize_x && (m.a_y * x + m.b_y * y) = m.prize_y);
      (3*x + y)

  end
  else 
    0


let etape2 () = 
  let machines = input_jour13_2 () in
  
  let total = ref 0 in 
  List.iter (fun m -> (*Printf.printf "%d\n" (nb_jetons m)*) total := !total + (nb_jetons_evolue m)) machines ; 
  Printf.printf "res2 = %d\n" !total


let _ = etape2 ()