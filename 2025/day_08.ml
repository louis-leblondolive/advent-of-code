type battery = {
    x : int ;
    y : int ; 
    z : int 
}
type battery_couple = {
    dist : float ; 
    bt_1 : int ; 
    bt_2 : int 
}

let battery_dist b1 b2 = 
    let dx = b1.x - b2.x in 
    let dy = b1.y - b2.y in 
    let dz = b1.z - b2.z in 
    float_of_int ( dx*dx + dy*dy + dz*dz )


let build_battery_list input = 

    let rec get_battery_list file =
        match input_line file with 
        | str -> Scanf.sscanf str "%d,%d,%d" (fun xb yb zb -> {x = xb; y = yb; z = zb})  :: get_battery_list file 
        | exception End_of_file -> []
    in
    get_battery_list input


let puzzle_01 depth bt_list_raw  = 

    let nb_bat = List.length bt_list_raw in  

    (* union-find system *)
    let circuits = Array.mapi (fun i _ -> -(i+1)) (Array.make nb_bat 0) in 

    let find_circuit i = 
        (* returns the circuit containing battery i *)
        let pos = ref i in 
        while circuits.(!pos) >= 0 do 
            pos := circuits.(!pos)
        done ; 

        let root = !pos in 
        pos := i ; 
        while circuits.(!pos) >= 0 do 
            let cache = circuits.(!pos) in
            circuits.(!pos) <- root ; 
            pos := cache 
        done ; 
        -circuits.(!pos)
    in

    let merge_circuits i j = 
        let ri = find_circuit i - 1 in  
        let rj = find_circuit j - 1 in  
        if ri <> rj then
            circuits.(ri) <- rj  (* fusion simple, sans union par rang *)
    in

    (* test union find *)
    (* merge_circuits 1 10 ;  *)
    (* Printf.printf "%d\n" (find_circuit 1) ; *)
    (* Array.iter (fun n -> Printf.printf "%d " n) circuits ; print_newline () ; *)

    (* input post processing *)
    let mat_bat = Array.make nb_bat {x = 0; y = 0; z = 0} in 
    List.iteri (fun i bt -> mat_bat.(i) <- bt) bt_list_raw; 
    
    let battery_list = ref [] in 

    for i = 0 to nb_bat - 1 do 
        for j = 0 to i - 1 do  

            let d = battery_dist mat_bat.(i) mat_bat.(j) in
            battery_list := {dist = d ; bt_1 = i ; bt_2 = j} :: !battery_list

        done
    done ; 

    (* battery sort and merge *)
    let battery_couple_cmp bc1 bc2 =
        if bc1.dist = bc2.dist then 0 
        else if bc1.dist > bc2.dist then 1 
        else -1 
    in

    let bt_list_sorted = ref (List.sort battery_couple_cmp !battery_list) in 

    for _ = 0 to depth - 1 do 
        let b_cpl = List.hd !bt_list_sorted in 

        merge_circuits b_cpl.bt_1 b_cpl.bt_2 ; 

        bt_list_sorted := List.tl !bt_list_sorted
    done ;


    (* calculate circuit size *)
    let circuits_sizes_tbl = Hashtbl.create 10000 in 
    for i = 0 to nb_bat - 1 do 
       let id_i = find_circuit i in 
        if Hashtbl.mem circuits_sizes_tbl id_i then 
            Hashtbl.replace circuits_sizes_tbl id_i (Hashtbl.find circuits_sizes_tbl id_i + 1)
        else
            Hashtbl.add circuits_sizes_tbl id_i 1 
    done ; 
    
    let circuits_sizes_list = ref [] in 
    Hashtbl.iter (fun _ v -> circuits_sizes_list := v :: !circuits_sizes_list) circuits_sizes_tbl; 
    circuits_sizes_list := List.sort (fun a b -> - Stdlib.compare a b) !circuits_sizes_list ;  

    match !circuits_sizes_list with 
    | a :: b :: c :: _ -> Printf.printf "Puzzle 01 : %d \n" (a*b*c)
    | _ -> ()



exception FoundLastBattery of battery_couple
let puzzle_02 bt_list_raw  = 

    let nb_bat = List.length bt_list_raw in  

    (* union-find system *)
    let circuits_cpt = ref nb_bat in 
    let circuits = Array.mapi (fun i _ -> -(i+1)) (Array.make nb_bat 0) in 

    let find_circuit i = 
        (* returns the circuit containing battery i *)
        let pos = ref i in 
        while circuits.(!pos) >= 0 do 
            pos := circuits.(!pos)
        done ; 

        let root = !pos in 
        pos := i ; 
        while circuits.(!pos) >= 0 do 
            let cache = circuits.(!pos) in
            circuits.(!pos) <- root ; 
            pos := cache 
        done ; 
        -circuits.(!pos)
    in

    let merge_circuits i j = 
        let ri = find_circuit i - 1 in  
        let rj = find_circuit j - 1 in  
        if ri <> rj then begin 
            decr circuits_cpt ; 
            circuits.(ri) <- rj  end
    in

    (* input post processing *)
    let mat_bat = Array.make nb_bat {x = 0; y = 0; z = 0} in 
    List.iteri (fun i bt -> mat_bat.(i) <- bt) bt_list_raw; 
    
    let battery_list = ref [] in 

    for i = 0 to nb_bat - 1 do 
        for j = 0 to i - 1 do  

            let d = battery_dist mat_bat.(i) mat_bat.(j) in
            battery_list := {dist = d ; bt_1 = i ; bt_2 = j} :: !battery_list

        done
    done ; 

    (* battery sort and merge *)
    let battery_couple_cmp bc1 bc2 =
        if bc1.dist = bc2.dist then 0 
        else if bc1.dist > bc2.dist then 1 
        else -1 
    in

    let bt_list_sorted = ref (List.sort battery_couple_cmp !battery_list) in 

    let last_used_battery_cpl = ref (List.hd !bt_list_sorted) in 

    while !circuits_cpt > 1 do 
        let b_cpl = List.hd !bt_list_sorted in 

        last_used_battery_cpl := b_cpl ;
        merge_circuits b_cpl.bt_1 b_cpl.bt_2 ; 
        bt_list_sorted := List.tl !bt_list_sorted
    done ;
    
    Printf.printf "Puzzle 02 : %d \n" 
        (mat_bat.(!last_used_battery_cpl.bt_1).x * mat_bat.(!last_used_battery_cpl.bt_2).x)




let main () = 
    let depth = 1000  in 
    let bt_list = build_battery_list (open_in "input_08.txt") in 

    puzzle_01 depth bt_list  ; 
    puzzle_02 bt_list ;
    ()

let () = main ()