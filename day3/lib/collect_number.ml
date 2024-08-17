type t = { number : int; range : Range.t }

let is_asterisk = function '*' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false
let get_number { number; _ } = number
let get_range { range; _ } = range

let get_number_with_properties idx char_list =
  let number_len = List.length char_list in
  if number_len = 0 then None
  else
    let number =
      char_list |> Iter.of_list |> Iter.rev |> Iter.to_str |> int_of_string
    in
    Some { number; range = Range.(idx - number_len - 1 -- idx) }

let append_number_with_properties idx char_list numbers_with_properties =
  Option.fold ~none:numbers_with_properties
    ~some:(fun res -> res :: numbers_with_properties)
    (get_number_with_properties idx char_list)

let parse_line line =
  let _, asterisk_positions, numbers_with_properties, acc_for_number =
    String.fold_left
      (fun (idx, asterisk_positions, numbers_with_properties, acc_for_number) ch ->
        match (is_digit ch, is_asterisk ch) with
        | false, false ->
            ( idx + 1,
              asterisk_positions,
              append_number_with_properties idx acc_for_number
                numbers_with_properties,
              [] )
        | _, true ->
            ( idx + 1,
              idx :: asterisk_positions,
              append_number_with_properties idx acc_for_number
                numbers_with_properties,
              [] )
        | true, _ ->
            ( idx + 1,
              asterisk_positions,
              numbers_with_properties,
              ch :: acc_for_number ))
      (0, [], [], []) line
  in
  let numbers_with_properties =
    append_number_with_properties
      (String.length line - 1)
      acc_for_number numbers_with_properties
  in
  (asterisk_positions, numbers_with_properties)
