type drive = {
  disk : int array ;
  void : int Queue.t ; 
  files : int Stack.t
}

let affiche_tab t = 
  Array.iter (Printf.printf "%d ") t ; 
  print_newline ()

let char_to_int c = 
  int_of_char c - 48


let image_disque () = 
  let f = open_in "input09.txt" in 
  let s = input_line f in 
  let n = String.length s in 
  
  let len = ref 0 in 
  String.iter (fun c -> len := !len + char_to_int c) s ; 

  let disque = Array.make !len (-1) in
  let vide = Queue.create () in
  let fichiers = Stack.create () in 

  let cpt =  ref 0 in 
  for i = 0 to n - 1 do 

    if i mod 2 = 0 then begin 
      for j = !cpt to !cpt + (char_to_int s.[i]) - 1 do 
        disque.(j) <- i / 2 ;
        Stack.push j fichiers
      done
    end 
    else begin 
      for j = !cpt to !cpt + (char_to_int s.[i]) - 1 do 
        Queue.add j vide
      done
    end ; 

    cpt := !cpt + char_to_int s.[i]
  done ; 
  (*affiche_tab disk ;*)
  {disk = disque ; void = vide ; files = fichiers}


let checksum d = 
  let cpt = ref 0 in 
  for i = 0 to Array.length d - 1 do 
    if d.(i) <> -1 then 
      cpt := !cpt + (i * d.(i)) 
  done ; 
  !cpt


(* PARTIE I *)
exception Invalide
let etape1 () = 
  let dd = image_disque () in


  let dd_valide d = 
    let b = ref false in 
    try
      for i = 0 to Array.length d.disk - 1 do 
        if not !b then 
          b := d.disk.(i) = (-1) 
        else 
          if d.disk.(i) <> -1 then 
            raise Invalide
      done ; 
      true
    with 
    | Invalide -> false
  in

  while not (Queue.is_empty dd.void) && not (dd_valide dd) do 
    let f = Stack.pop dd.files in
    let v = Queue.take dd.void in

    dd.disk.(v) <- dd.disk.(f) ;
    dd.disk.(f) <- (-1) 
  done ;
  
  Printf.printf "res1 = %d \n" (checksum dd.disk)


(* PARTIE II *)
type file = {
  mutable st : int ;
  mutable nd : int ;
  index : int 
}

type vide = {
  mutable deb : int ;
  mutable fin : int
}

type evolved_drive = {
  disk : int array ; 
  files : file Stack.t ; 
  voids : vide list
}

let affiche_disque d =
  Printf.printf "vides : \n" ; 
  List.iter (fun v -> Printf.printf "%d %d \n" v.deb v.fin) d.voids ; 
  Printf.printf "fichiers : \n" ;
  Stack.iter (fun f -> Printf.printf "i = %d ; %d %d \n" f.index f.st f.nd) d.files ;
  Printf.printf "\n" 

let image_disque_evoluee () = 
  let f = open_in "input09.txt" in 
  let s = input_line f in 
  let n = String.length s in 
  
  let len = ref 0 in 
  String.iter (fun c -> len := !len + char_to_int c) s ; 

  let disque = Array.make !len (-1) in
  let vide = ref [] in
  let fichiers = Stack.create () in 

  let cpt =  ref 0 in 
  for i = 0 to n - 1 do 

    if i mod 2 = 0 then begin (* on lit un fichier *)
      for j = !cpt to !cpt + (char_to_int s.[i]) - 1 do 
        disque.(j) <- i / 2 ;
      done; 
      Stack.push {st = !cpt ; nd = !cpt + (char_to_int s.[i]) - 1 ; index = i / 2} fichiers
    end 

    else begin (* on lit du vide *)
        vide :=  {deb = !cpt ; fin = !cpt + (char_to_int s.[i]) - 1} :: !vide
    end ; 

    cpt := !cpt + char_to_int s.[i]
  done ; 
  (*affiche_tab disk ;*)
  {disk = disque ; voids = List.rev !vide ; files = fichiers}


let etape2 () = 
  let dd = image_disque_evoluee () in

  let rec insere_fichier f vides =
    match vides with 
    | [] -> () 
    | vi :: tl -> if vi.deb > f.st then 
                        ()
                  else begin
                    if vi.fin - vi.deb >= f.nd - f.st then begin
                      (* copie fichier *)
                      for i = vi.deb to vi.deb + (f.nd - f.st) do 
                        dd.disk.(i) <- f.index 
                      done ; 
                      (* delete fichier *)
                      for i = f.st to f.nd do 
                        dd.disk.(i) <- (-1) 
                      done;
                      (* update vide *)
                      vi.deb <- vi.deb + (f.nd - f.st) + 1 
                    end 
                      else
                     insere_fichier f tl  
                  end
  in

  (*
  affiche_disque dd ;
  affiche_tab dd.disk ;*)

  while not (Stack.is_empty dd.files) do 
    let f = Stack.pop dd.files in
    insere_fichier f dd.voids
  done ; 
  (*
  affiche_disque dd ;
  affiche_tab dd.disk;*)

  Printf.printf "res2 = %d \n" (checksum dd.disk)

let _ = etape1 () ;
        etape2 ()