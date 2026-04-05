type equation = {res : int ; tests : int list}

let affiche_tab tab = 
  Array.iter (Printf.printf "%d %!") tab ; 
  Printf.printf "\n"

let incr_bin num = 
  let i = ref 0 in 
  let n = Array.length num in 

  while !i < n && num.(!i) = 1 do 
    num.(!i) <- 0 ; 
    incr i
  done ; 
  if !i < n then 
    num.(!i) <- 1
  else 
    num.(0) <- 42 

exception Solvable
let est_solvable test res = 
  let valeurs = Array.of_list test in
  let n = Array.length valeurs in 
  let cpt = Array.make (n-1) 0 in

  try 
    while cpt.(0) <> 42 do 
      let cache = ref (valeurs.(0)) in 

      for i = 1 to n - 1 do 
        if cpt.(i - 1) = 0 then 
         cache := !cache + valeurs.(i) 
        else 
          cache := !cache * valeurs.(i)
     done ; 
     
     if !cache = res then raise Solvable ;
     incr_bin cpt 

   done ; 
   Printf.printf "Non solvable %d\n" res ;
   false
  with 
  | Solvable -> Printf.printf "solvable %d\n" res ; true


let rec exp a n =
  if n = 0 then 1
  else a * (exp a (n-1))

let concat a b = 
  (* renvoie a || b *)
  let offset = String.length (string_of_int b) in
  let a' = a * (exp 10 offset) in
  a' + b

let est_solvable_BESTIAL tests res = 
   
  let rec aux_BESTIAL acc valeurs = 
    match valeurs with 
    | [] -> false
    | [x] -> (x + acc = res) || (x*acc) = res || (concat acc x) = res

    | hd :: tl -> aux_BESTIAL (acc + hd) tl ||
                  aux_BESTIAL (acc * hd) tl ||
                  aux_BESTIAL (concat acc hd) tl
  in
  aux_BESTIAL 0 tests
    
  


let input_jour07 () = 
  let f = open_in "input07.txt" in
  let eqs = ref [] in 
  let rec aux_input f = 
    match input_line f with 
    | str -> let l = String.split_on_char ':' str in
             let re = int_of_string (List.hd l) in

             let tl = List.hd (List.tl l) in (* deuxième moitié*)
             let tl' = String.sub tl 1 (String.length tl - 1) in
             let te = List.map int_of_string ( String.split_on_char ' ' tl' ) in

             eqs := {res = re ; tests = te} :: !eqs ;
             aux_input f

    | exception End_of_file -> ()
  in

  aux_input f;
  !eqs


let etape1 () = 
  Printf.printf "hello world%!\n" ; 
  let eqs = input_jour07 () in 
  Printf.printf "input ok%!\n" ; 

  let cpt = ref 0 in 
  List.iter (fun eq -> if (est_solvable eq.tests eq.res) then cpt := !cpt + eq.res) eqs ; 

  Printf.printf "res1 = %d\n%!" !cpt

let etape2 () = 
  let eqs = input_jour07 () in 
  let cpt = ref 0 in 
  List.iter (fun eq -> if (est_solvable_BESTIAL eq.tests eq.res) then cpt := !cpt + eq.res) eqs;
  Printf.printf "res2 = %d\n%!" !cpt


let () = etape1 () ; 
         etape2 ()