type acc = {
  prev_symbols_positions : int list;
  prev_numbers : Lib.Collect_number.t list;
  sum : int;
}

let { sum; _ } =
  Iter.IO.lines_of "input.txt"
  |> Iter.map (fun line ->
         let symbols_adjacent_numbers_positions, numbers_with_properties =
           Lib.Collect_number.parse_line line
         in
         (symbols_adjacent_numbers_positions, numbers_with_properties))
  |> Iter.foldi
       (fun acc idx (symbols_adjacent_numbers_positions, numbers_with_properties) ->
         if idx = 0 then
           {
             acc with
             prev_symbols_positions = symbols_adjacent_numbers_positions;
             prev_numbers = numbers_with_properties;
           }
         else
           let current_symbols_position = symbols_adjacent_numbers_positions in
           let all_symbols_positions =
             List.append acc.prev_symbols_positions current_symbols_position
           in
           let sum = ref 0 in
           acc.prev_numbers
           |> List.iter (fun number_with_properties ->
                  if
                    all_symbols_positions
                    |> List.exists (fun position ->
                           Lib.Range.contains position
                             (Lib.Collect_number.get_range number_with_properties))
                  then
                    sum :=
                      !sum + Lib.Collect_number.get_number number_with_properties);
           let current_numbers =
             numbers_with_properties
             |> List.filter (fun number_with_properties ->
                    if
                      all_symbols_positions
                      |> List.exists (fun position ->
                             Lib.Range.contains position
                               (Lib.Collect_number.get_range
                                  number_with_properties))
                    then (
                      sum :=
                        !sum
                        + Lib.Collect_number.get_number number_with_properties;
                      false)
                    else true)
           in
           {
             prev_symbols_positions = current_symbols_position;
             prev_numbers = current_numbers;
             sum = acc.sum + !sum;
           })
       { prev_symbols_positions = []; prev_numbers = []; sum = 0 }

let _ = Format.printf "Sum is %i\n" sum
