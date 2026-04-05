(* utilities *)
let list_to_array l = 
  let n = List.length l in 
  let tab = Array.make n (List.hd l) in 
  List.iteri (fun i e -> tab.(i) <- e) l ;
  tab

let max a b = 
  if a > b then a else b 

let min a b = 
  if a > b then b else a 

let modulo a b = 
  (* renvoie a modulo b *)
  if a >= 0 then 
    a mod b 
  else
    let a' = ref a in 
    while !a' < 0 do 
      a' := !a' + b
    done ; 
    !a' mod b



let arr_max tab =
  let m = ref tab.(0) in
  for i = 1 to Array.length tab - 1 do 
    if tab.(i) > !m then m := tab.(i)
  done ; 
  !m 



(* puzzle *)
let puzzle_01 tiles_list = 
  
  let tiles_tab = list_to_array tiles_list in   
  let n = Array.length tiles_tab in 

  let max_surface = Array.make n 0 in 

  for i = 0 to n - 1 do 

    let t_i_x, t_i_y = tiles_tab.(i) in

    for j = 0 to i - 1 do 
      let t_j_x, t_j_y = tiles_tab.(j) in
      
      max_surface.(i) <- max max_surface.(i) (abs ((t_i_x - t_j_x + 1) * (t_i_y - t_j_y + 1)))
    done ; 
    for j = i + 1 to n - 1 do 
      let t_j_x, t_j_y = tiles_tab.(j) in

      max_surface.(i) <- max max_surface.(i) (abs ((t_i_x - t_j_x + 1) * (t_i_y - t_j_y + 1)))
    done 
  done ; 

  arr_max max_surface


exception Point_In_Polygon  
exception Edge_Intersect
let puzzle_02 tiles_list = 
  
  let tiles_tab = list_to_array tiles_list in 
  let n = Array.length tiles_tab in 
  let max_surfaces = Array.make n (-1) in 

  (* rectangle in enclosure test *)
  let point_in_polygon x y tiles_tab = 
      let n = Array.length tiles_tab in 
      (* checks if x y is inside tiles_tab enclosure *)
      try 
        let intersect_cpt = ref 0 in 
        for i = 0 to n - 1 do 
          
          let x_i, y_i = tiles_tab.(i) in 
          let x_j, y_j = tiles_tab.((i + 1) mod n) in 
          
          (* on the border ? *)
          if min x_i x_j <= x && x <= max x_i x_j && min y_i y_j <= y && y <= max y_i y_j then raise Point_In_Polygon ;

          (* does the ray intersects edge i - j ? *)
          if (y_i > y) <> (y_j > y) then begin (* does i - j straddle the ray ? *)
            if x_i = x_j && x_i <= x then incr intersect_cpt ;
            if x_i <> x_j && y_i = y && max x_i x_j < x then incr intersect_cpt ; 
          end;

        done ;
        !intersect_cpt mod 2 = 1

      with 
      | Point_In_Polygon -> true 
  in

  (* test : *)
  (*
  let x_max = Array.fold_left (fun acc (x, _) -> max acc x) 0 tiles_tab + 2 in
  let y_max = Array.fold_left (fun acc (_, y) -> max acc y) 0 tiles_tab + 2 in

  Printf.printf "%d %d \n" x_max y_max ; 

  let map = Array.make y_max [||] in 
  for i = 0 to y_max - 1 do map.(i) <- Array.make x_max '.' done ;

  for i = 0 to n - 1 do 
    let x, y = tiles_tab.(i) in 
    map.(y).(x) <- '#'
  done ; 


  for x = 0 to x_max - 1 do 
    for y = 0 to y_max - 1 do

      if point_in_polygon x y tiles_tab && map.(y).(x) <> '#' then 
        map.(y).(x) <- 'X' 

    done 
  done; 

  for y = 0 to y_max - 1 do 
    for x = 0 to x_max - 1 do 
      Printf.printf "%c" map.(y).(x) 
    done ; 
    print_newline () 
  done ; 
  *)

  let edges_intersect r_x1 r_y1 r_x2 r_y2 tiles_tab =
      let n = Array.length tiles_tab in 
      (* checks if edge R1 - R2 intersects with any edge of tiles_tab *)
      try 
        for i = 0 to n - 1 do 
        
          let p_xi, p_yi = tiles_tab.(i) in 
          let p_xj, p_yj = tiles_tab.((i + 1) mod n) in 

          if p_yi <> p_yj && r_x1 <> r_x2 then 
            
            if min r_x1 r_x2 < p_xi && p_xi < max r_x1 r_x2 
            && min r_x1 r_x2 < p_xj && p_xj < max r_x1 r_x2
            && r_y1 < max p_yi p_yj && r_y2 < max p_yi p_yj 
            && r_y1 > min p_yi p_yj && r_y2 > min p_yi p_yj then raise Edge_Intersect;

          if p_xi <> p_xj && r_y1 <> r_y2 then 

            if min r_y1 r_y2 < p_yi && p_yi < max r_y1 r_y2 
            && min r_y1 r_y2 < p_yj && p_yj < max r_y1 r_y2
            && r_x1 < max p_xi p_xj && r_x2 < max p_xi p_xj 
            && r_x1 > min p_xi p_xj && r_x2 > min p_xi p_xj then raise Edge_Intersect;

        done ; 
        true 
      with
      | Edge_Intersect -> false 
  in

  (*test*)
  let test_edges_intersect () =
  let ok = ref 0 in
  let fail = ref 0 in
  let check label r_x1 r_y1 r_x2 r_y2 tiles expected =
    let result = edges_intersect r_x1 r_y1 r_x2 r_y2 tiles in
    if result = expected then begin
      Printf.printf "OK  : %s\n" label;
      incr ok
    end else begin
      Printf.printf "FAIL: %s — expected %b got %b\n" label expected result;
      incr fail
    end
  in

  (* Cas 1 : R horizontale, aucune intersection *)
  check "R horiz, no intersect"
    0 5 10 5
    [|(0,0);(10,0);(10,3);(0,3)|]
    true;

  (* Cas 2 : R horizontale, extrémités sur les arêtes verticales du polygone *)
  check "R horiz, endpoints on polygon vertical edges"
    0 5 10 5
    [|(0,0);(10,0);(10,10);(0,10)|]
    true;  (* r_x1=0 et r_x2=10 sont sur les bords du polygone *)

  (* Cas 3 : R verticale, extrémités sur les arêtes horizontales du polygone *)
  check "R vert, endpoints on polygon horizontal edges"
    5 0 5 10
    [|(0,0);(10,0);(10,10);(0,10)|]
    true;  (* r_y1=0 et r_y2=10 sont sur les bords du polygone *)

  (* Cas 4 : R verticale, aucune intersection *)
  check "R vert, no intersect"
    5 0 5 3
    [|(0,0);(3,0);(3,10);(0,10)|]
    true;

  (* Cas 5 : R horizontale passe exactement par un sommet du polygone *)
  check "R horiz touches polygon vertex"
    0 5 10 5
    [|(0,0);(5,5);(10,0);(10,10);(0,10)|]
    true;

  (* Cas 6 : R coïncide avec une arête horizontale du polygone *)
  check "R horiz coincides with polygon edge"
    0 0 10 0
    [|(0,0);(10,0);(10,10);(0,10)|]
    true;

  (* Cas 7 : R coïncide avec une arête verticale du polygone *)
  check "R vert coincides with polygon edge"
    0 0 0 10
    [|(0,0);(10,0);(10,10);(0,10)|]
    true;

  (* Cas 8 : R horizontale, arête verticale du polygone touche exactement r_x1 ou r_x2 *)
  check "R horiz, polygon vert edge at boundary x"
    2 5 8 5
    [|(2,0);(2,10);(8,10);(8,0)|]
    true;

  (* Cas 9 : R verticale, arête horizontale du polygone touche exactement r_y1 ou r_y2 *)
  check "R vert, polygon horiz edge at boundary y"
    5 2 5 8
    [|(0,2);(10,2);(10,8);(0,8)|]
    true;


  (* R traverse vraiment une arête verticale du polygone *)
  check "R horiz truly crosses vertical polygon edge"
    3 5 8 5
    [|(5,0);(5,10);(10,10);(10,0)|]
    false;

  (* R traverse vraiment une arête horizontale du polygone *)
  check "R vert truly crosses horizontal polygon edge"
    5 3 5 8
    [|(0,5);(10,5);(10,10);(0,10)|]
    false;

  Printf.printf "\n%d/%d passed\n" !ok (!ok + !fail) in 
  (* test_edges_intersect () ; *)



  let rectangle_in_enclosure t_i_x t_i_y t_j_x t_j_y tiles_tab =
    
    let rect_vert = [|
      (min t_i_x t_j_x, min t_i_y t_j_y);
      (max t_i_x t_j_x, min t_i_y t_j_y);
      (max t_i_x t_j_x, max t_i_y t_j_y);
      (min t_i_x t_j_x, max t_i_y t_j_y);
    |] in 

    let vert_in = Array.fold_left (fun b (x, y) -> b && point_in_polygon x y tiles_tab) true rect_vert in 

    let edge_in () =
      let b = ref true in 
      for i = 0 to 3 do 
        let r1x, r1y = rect_vert.(i) in 
        let r2x, r2y = rect_vert.((i+1) mod 4) in 
        b := !b && edges_intersect r1x r1y r2x r2y tiles_tab 
      done ; 

      !b 
    in

    vert_in && edge_in ()
  in
    
  for i = 0 to n - 1 do 

    let t_i_x, t_i_y = tiles_tab.(i) in 

    for j = 0 to i - 1 do 
      let t_j_x, t_j_y = tiles_tab.(j) in 

      if rectangle_in_enclosure t_i_x t_i_y t_j_x t_j_y tiles_tab then 
        max_surfaces.(i) <- max max_surfaces.(i) (abs ((t_i_x - t_j_x + 1) * (t_i_y - t_j_y + 1)))
    done;

    for j = i + 1 to n - 1 do 
      let t_j_x, t_j_y = tiles_tab.(j) in 

      if rectangle_in_enclosure t_i_x t_i_y t_j_x t_j_y tiles_tab then 
        max_surfaces.(i) <- max max_surfaces.(i) (abs ((t_i_x - t_j_x + 1) * (t_i_y - t_j_y + 1)))
    done
  done ;
  
  let candidates = ref [] in

for i = 0 to n - 1 do
  let t_i_x, t_i_y = tiles_tab.(i) in
  for j = 0 to n - 1 do
    if i <> j then begin
      let t_j_x, t_j_y = tiles_tab.(j) in
      if rectangle_in_enclosure t_i_x t_i_y t_j_x t_j_y tiles_tab then begin
        let s = abs ((t_i_x - t_j_x + 1) * (t_i_y - t_j_y + 1)) in
        candidates := (s, t_i_x, t_i_y, t_j_x, t_j_y) :: !candidates
      end
    end
  done
done ;

let sorted = List.sort (fun (s1,_,_,_,_) (s2,_,_,_,_) -> compare s2 s1) !candidates in
List.iteri (fun i (s,x1,y1,x2,y2) ->
  if i < 20 then Printf.printf "rect (%d,%d)-(%d,%d) surface=%d\n" x1 y1 x2 y2 s
) sorte;

  arr_max max_surfaces








let main () = 

  let rec read_line file = 
    match input_line file with 
    | str -> Scanf.sscanf str "%d,%d" (fun i j -> (i, j)) :: read_line file 
    | exception End_of_file -> []
  in

  let file = open_in "input_09.txt" in 
  let tiles_list = read_line file in 

  Printf.printf "Puzzle 01 : max surface = %d \n" (puzzle_01 tiles_list);
  Printf.printf "Puzzle 02 : max surface = %d \n" (puzzle_02 tiles_list)

let () = main ()