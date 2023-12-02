type cubes = { red : int; green : int; blue : int }

let get_fewer_numbers_cubes_for_game g =
  g.Lib.Helpers.subsets
  |> List.fold_left
       (fun acc subset ->
         subset
         |> List.fold_left
              (fun (acc : cubes) (cube : Lib.Helpers.cube) ->
                match cube with
                | Lib.Helpers.Red count ->
                    if count > acc.red then { acc with red = count } else acc
                | Green count ->
                    if count > acc.green then { acc with green = count }
                    else acc
                | Blue count ->
                    if count > acc.blue then { acc with blue = count } else acc)
              acc)
       { red = 0; green = 0; blue = 0 }

let game_sum =
  Iter.IO.lines_of "input.txt"
  |> Iter.map (fun line ->
         match
           Angstrom.parse_string ~consume:All Lib.Helpers.game_line line
         with
         | Ok v -> v
         | Error msg -> failwith msg)
  |> Iter.map get_fewer_numbers_cubes_for_game
  |> Iter.map (fun { red; green; blue } -> red * green * blue)
  |> Iter.sum
;;

print_endline ("Result: " ^ string_of_int game_sum ^ "\n")
