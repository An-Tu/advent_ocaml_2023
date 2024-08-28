module IntMap = Map.Make (struct
  type t = int

  let compare = compare
end)

let pp_int_list fmt list =
  let open Format in
  fprintf fmt "[ %a ]"
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_print_int)
    list

let () =
  let cards_in_total, _ =
    Iter.IO.lines_of "input.txt"
    |> Iter.map (Angstrom.parse_string ~consume:All Lib.Parser.line)
    |> Iter.fold
         (fun (cards_in_total, acc) parsing_results ->
           match parsing_results with
           | Ok (el : Lib.Parser.t) ->
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

               let card_copies_number = !res |> IntMap.find el.card_number in
               for i = match_count downto 1 do
                 res :=
                   !res
                   |> IntMap.update (el.card_number + i) (fun value ->
                          match value with
                          | None -> Some card_copies_number
                          | Some x -> Some (x + card_copies_number))
               done;
               ( cards_in_total + card_copies_number,
                 !res |> IntMap.remove el.card_number )
           | Error msg -> failwith msg)
         (0, IntMap.empty)
  in

  print_endline ("Result: " ^ (cards_in_total |> string_of_int))
