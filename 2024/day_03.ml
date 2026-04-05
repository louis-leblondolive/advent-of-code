let etape1 li = 
    let cpt = ref 0 in
    let i = ref 0 in 

    while !i < String.length li - 10 do 

        for j = 8 to (min (String.length li - !i - 1) 12) do

            let li' = String.sub li !i j in
            (*Printf.printf "%s\n" li' ;*)
            (*Printf.printf "%d\n" (min (String.length li - !i - 1) 12) ;*)
            
            try 
                Scanf.sscanf li' "mul(%d,%d)" (fun x y -> cpt := !cpt + x*y) ;
                (*Printf.printf "VALIDE \n" ;*)
                Printf.printf "res1 = %d \n" !cpt ;
                incr i
            with 
            | Scanf.Scan_failure exn -> incr i 
            | End_of_file -> ()
        done 
        
    done 

let etape2 li = 
    let cpt = ref 0 in 
    let i = ref 0 in 
    let enabled = ref true in

    while !i < String.length li - 12 do 
        
            let li' = String.sub li !i 12 in
            (*Printf.printf "%s  --  %b \n" li' !enabled ;*)

            try 
                if li.[!i] = 'm' && !enabled then begin
                    Scanf.sscanf li' "mul(%d,%d)" (fun x y -> cpt := !cpt + x*y) ;
                    Printf.printf "             ------- VALIDE : cpt = %d \n" !cpt 
                end
                else if li.[!i] = 'd' && li.[!i + 2] = 'n' then begin
                    Scanf.sscanf li' "don't()" (enabled := false) ;
                    (*Printf.printf "             ------- Bloquage \n"*)
                    
                end
                else if li.[!i] = 'd' && li.[!i + 1] = 'o' && li.[!i + 2] = '(' && li.[!i + 3] = ')' then begin
                    Scanf.sscanf li' "do()" (enabled := true) ;
                    (*Printf.printf "             ------- Débloquage \n"*)
                end ;
                incr i 
            with 
            | Scanf.Scan_failure exn -> incr i
            | End_of_file -> ()
    done 
        
    
let () = 
    let entree = open_in "input03.txt" in
    etape2 (input_line entree)