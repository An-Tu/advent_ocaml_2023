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
      let res =
        v
        |> List.fold_left
             (fun acc (el : Lib.Parser.t) ->
               let res =
                 ref
                   (acc
                   |> IntMap.update el.card_number (function
                        | None -> Some 1
                        | Some x -> Some (x + 1)))
               in
               let winning_numbers_map =
                 el.winning_numbers
                 |> List.map (fun num -> (num, ()))
                 |> IntMap.of_list
               in
               let match_count =
                 el.our_numbers
                 |> List.fold_left
                      (fun acc num ->
                        match IntMap.mem num winning_numbers_map with
                        | false -> acc
                        | true -> acc + 1)
                      0
               in
               for i = match_count downto 1 do
                 res :=
                   !res
                   |> IntMap.update (el.card_number + i) (fun value ->
                          let add_count = !res |> IntMap.find el.card_number in
                          match value with
                          | None -> Some add_count
                          | Some x -> Some (x + add_count))
               done;
               !res)
             IntMap.empty
        |> IntMap.to_list
        |> List.fold_left (fun acc (num, count) -> acc + count) 0
      in
      print_endline ("Result: " ^ (res |> string_of_int))
  | Error msg -> failwith msg
