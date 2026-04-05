type stone = int list

let affiche l = 
  List.iter (Printf.printf "%d ") l ; 
  Printf.printf "\n"

let affiche_b l = 
  List.iter (fun s -> affiche s) l ; 
  Printf.printf "\n"

let input_jour_11 () = 
  (*[[1;2;5]; [1;7]]*)
  [[0]; [2;7]; [5;4;0;9;9;3;0]; [8;2;8;9;7;9]; [4;4;7;1]; [3]; [6;8;5;2;4]; [1;7;0]]

let somme x y = 

  let rec aux_somme a b ret = 
    match (a, b) with 
    | ([], []) -> if ret then [1]
                  else []
    | (hd :: tl, []) -> if ret then begin
                          if hd + 1 <= 9 then (hd+1) :: tl

                          else 0 :: (aux_somme (tl) [] true) 

                        end
                        else 
                          a

    | ([], _) -> aux_somme b a ret

    | (hd_a :: tl_a , hd_b :: tl_b) -> if not ret then begin 
                                        if hd_a + hd_b <= 9 then 
                                          (hd_a + hd_b) :: (aux_somme tl_a tl_b false)
                                        else 
                                          (hd_a + hd_b - 10) :: (aux_somme tl_a tl_b true)
                                        end 
                                      else begin (* retenue *)
                                        if hd_a + hd_b + 1 <= 9 then 
                                          (hd_a + hd_b + 1) :: (aux_somme tl_a tl_b false)
                                        else
                                          (hd_a + hd_b + 1 - 10) :: (aux_somme tl_a tl_b true)
                                      end
  in

  aux_somme x y false

let rule0 () = 
  [1]

let rule1 s' = 

  let s = List.rev s' in 

  let rec aux_x_2024 fact x ret = 
    match x with 
    | [] -> if ret <> 0 then [ret]
            else []
    | hd :: tl -> ( (ret + hd * fact) mod 10 ) :: aux_x_2024 fact tl ( (ret + hd * fact) / 10 ) 
  in 

  let x1 = aux_x_2024 4 s 0 in
  let x2 = aux_x_2024 2 (0 :: s) 0 in
  let x3 = aux_x_2024 2 (0 :: 0 :: 0 :: s) 0 in 
  
  let r1 = somme x1 x2 in 
  let r2 = somme x3 r1 in 
  List.rev r2

let rule2 s = 
  let n = List.length s in 

  let rec aux_nettoie l = 
    match l with 
    | [hd] when hd = 0 -> [0]
    | hd :: tl when hd = 0 -> aux_nettoie tl
    | _ -> l
  in

  let rec aux_separe cpt acc l = 
    match l with 
    | [] -> (acc, [])
    | hd :: tl -> if cpt = n / 2 then (aux_nettoie (List.rev acc), aux_nettoie l)
                  else aux_separe (cpt+1) (hd :: acc) tl 
  in

  aux_separe 0 [] s 

(*
let () = 
  let a, b = rule2 [ 0; 0; 0; 1; 2; 3] in
  affiche a; 
  affiche b
*)


let etape1 () = 
  let stones = ref (input_jour_11 ()) in

  let rec blink l acc = 
    match l with 
    | [] -> acc

    | s :: tl when s = [0] -> blink tl ([1] :: acc)

    | s :: tl -> let n = List.length s in 
                 if n = 0 then 
                  blink tl acc
                 else if n mod 2 = 0 then begin
                    let s1, s2 = rule2 s in 
                    blink tl (s1 :: s2 :: acc)
                 end
                 else begin
                     blink tl ((rule1 s) :: acc)
                 end
  in

  for i = 0 to 24 do 
    stones := blink !stones [];
    (*affiche_b !stones*)
  done ; 
  Printf.printf "res1 = %d\n" (List.length !stones)


let etape2 () = 
  let stones = input_jour_11 () in

  let precalc = Hashtbl.create 100000 in 

  let rec blink s n =
    (* renvoie combien de pierres donne s en n blinks *)
    if n <= 0 then 
      1

    else begin
    match Hashtbl.find_opt precalc (s, n) with 
    | Some k -> k
    | None -> if s = [0] then begin
                let k = blink [1] (n - 1) in
                Hashtbl.add precalc (s, n) k ;
                k
              end

              else begin

                let l = List.length s in 
                if l mod 2 = 0 then begin
                  let s1, s2 = rule2 s in 
                  Hashtbl.add precalc (s, n) ((blink s1 (n-1)) + (blink s2 (n-1)));
                  (blink s1 (n-1)) + (blink s2 (n-1)) 
                end

                else begin 
                  let r = rule1 s in 
                  Hashtbl.add precalc (s, n) (blink r (n-1)) ; 
                  blink r (n-1)
                end
                
              end
      end
  in

  let cpt = ref 0 in 
  let ite = 75 in 
  List.iter (fun s -> cpt := !cpt + blink s ite) stones ; 

  Printf.printf "res2 = %d \n" !cpt


let _ = etape1 ();
        etape2 ()
