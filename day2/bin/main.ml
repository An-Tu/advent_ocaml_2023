let max_red = 12
let max_green = 13
let max_blue = 14

let is_possible_game g =
  if
    g.Lib.Helpers.subsets
    |> List.for_all (fun subset ->
           subset
           |> List.for_all (fun cube ->
                  match cube with
                  | Lib.Helpers.Red count when count <= max_red -> true
                  | Green count when count <= max_green -> true
                  | Blue count when count <= max_blue -> true
                  | _ -> false))
  then Some g.game_id
  else None

let game_sum =
  Iter.IO.lines_of "input.txt"
  |> Iter.map (fun line ->
         match
           Angstrom.parse_string ~consume:All Lib.Helpers.game_line line
         with
         | Ok v -> v
         | Error msg -> failwith msg)
  |> Iter.map (fun game_line ->
         match is_possible_game game_line with
         | None -> 0
         | Some game_id -> game_id)
  |> Iter.sum
;;

print_endline ("Result: " ^ string_of_int game_sum ^ "\n")
