let ecarts_niveaux l = 
  
  let rec aux_cons hd_prev l' acc = 
    match l' with 
    | [] -> acc
    | hd :: tl -> aux_cons hd tl ((hd_prev - hd) :: acc) 
  in

  aux_cons (List.hd l) (List.tl l) []

let safe_report l = 
  let diffs = ecarts_niveaux l in

  
  List.iter (fun i -> Printf.printf "%d " i) diffs ;
  Printf.printf "\n\n" ; 

  let hd_0 = List.hd diffs in 
  let ok = ref true in 

  let err_cpt = ref 0 in

  if hd_0 = 0 then 
    
    let hd_1 = List.hd (List.tl diffs) in
    List.iter (fun elt -> ok := !ok && (hd_1 * elt > 0) && (abs(elt) >= 1) && (abs(elt) <= 3)) (List.tl diffs) ;
    (abs(hd_1) <= 3) && (abs(hd_1) >= 1) && !ok

  else begin  
    List.iter (fun elt -> let verif = (hd_0 * elt > 0) && (abs(elt) >= 1) && (abs(elt) <= 3) in
                            if not verif then incr err_cpt)
    (List.tl diffs) ;

    if abs(hd_0) < 1 || abs(hd_0) > 3 then begin
      incr err_cpt
    end ;
    
    !err_cpt <= 1

  end


let etape1 l = 
  let cpt = ref 0 in 

  List.iter (fun r -> if safe_report r then incr cpt) l ;

  Printf.printf "res1 : %d \n" !cpt


let rec read_input file = 
  match input_line file with
	| str -> (List.map int_of_string (String.split_on_char ' ' str)) :: read_input file
	| exception End_of_file -> []


let main () = 
  let f = open_in "input02.txt" in
  let l = read_input f in

  etape1 l 

let _ = main ()