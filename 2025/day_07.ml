let format_input file = 

    let rec read_line file = 
      match input_line file with 
      | str -> str :: read_line file 
      | exception End_of_file -> [] 
    in

    let rec string_to_array s = 
      let tab = Array.make (String.length s) ' ' in 
      String.iteri (fun n c -> tab.(n) <- c) s;
      tab
    in

    let raw_input = read_line file in 
    let input = Array.make (List.length raw_input) [||] in 
    List.iteri (fun i s -> input.(i) <- string_to_array s) raw_input ; 

    input


let puzzle_01 data = 

  let grid = Array.map Array.copy data in 

  let split_count = ref 0 in 
  let n = Array.length grid in 

  for y = 1 to n - 1 do 
    let m = Array.length grid.(y) in 
    for x = 0 to m - 1 do 
      
      match grid.(y - 1).(x) with 
      | 'S' -> grid.(y).(x) <- '|' 

      | '|' -> if grid.(y).(x) = '^' then begin 
                  incr split_count ;
                  if x - 1 >= 0 then grid.(y).(x-1) <- '|';
                  if x + 1 < m then grid.(y).(x+1) <- '|'
               end
               else grid.(y).(x) <- '|'

      | _ -> ()

    done  
  done ; 
  !split_count


let puzzle_02 data = 
  let grid = Array.map Array.copy data in 

  let n = Array.length grid in 
  let timelines_count = Array.map (fun tab -> Array.make (Array.length tab) (-1)) grid in 

  let rec get_timeline_count y x = 
    if x < 0 || x >= Array.length grid.(y) || y < 0 || y > n - 1 then 0
    else begin 
    (* returns the number of timeline possible starting at (x,y) in the grid *)
      if timelines_count.(y).(x) >= 0 then timelines_count.(y).(x)

      else begin 
        if y = n - 1 then begin 
          timelines_count.(y).(x) <- 1; 
          1
        end 

        else begin 
          if grid.(y+1).(x) = '^' then begin 
            let count = get_timeline_count (y+1) (x-1) + get_timeline_count (y+1) (x+1) in 
            timelines_count.(y).(x) <- count ; 
            count end

          else begin 
            let count = get_timeline_count (y+1) x in 
            timelines_count.(y).(x) <- count ; 
            count end 

        end 
      end 
    end
  in 

  let get_start_position grid = 
    let res = ref 0 in 
    for i = 0 to Array.length grid.(0) - 1 do 
      if grid.(0).(i) = 'S' then res := i 
    done ;
    !res
  in 

  get_timeline_count 0 (get_start_position grid)






let main () = 

  let grid = format_input (open_in "input_07.txt") in 

  (* testing input *)
  (* Array.iter (fun a -> Array.iter (fun c -> print_char c ) a ; print_newline ()) grid  *)
  Printf.printf "Puzzle 01 : beam split %d times\n" (puzzle_01 grid) ; 
  Printf.printf "Puzzle 02 : %d timelines\n" (puzzle_02 grid)
 
  

let () = main ()