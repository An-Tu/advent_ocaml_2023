module IntMap = Map.Make (struct
  type t = int

  let compare = compare
end)

let read_file file = In_channel.with_open_bin file In_channel.input_all

let pp_int_list fmt list =
  let open Format in
  fprintf fmt "[ %a ]"
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_print_int)
    list

let () =
  let file = read_file "input.txt" in
  match Angstrom.parse_string ~consume:All Lib.Parser.parse_all file with
  | Ok v ->
      let sum =
        v
        |> List.fold_left
             (fun acc (el : Lib.Parser.t) ->
               let winning_numbers_map =
                 el.winning_numbers
                 |> List.map (fun num -> (num, ()))
                 |> IntMap.of_list
               in
               acc
               + (el.our_numbers
                 |> List.fold_left
                      (fun acc num ->
                        match IntMap.mem num winning_numbers_map with
                        | false -> acc
                        | true -> if acc = 0 then 1 else acc * 2)
                      0))
             0
      in
      print_endline ("Result: " ^ (sum |> string_of_int))
  | Error msg -> failwith msg
