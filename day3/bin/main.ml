let is_digit = function '0' .. '9' -> true | _ -> false
let is_dot = function '.' -> true | _ -> false

type acc = {
  prev_symbols_positions : int list;
  prev_numbers : Lib.Engine_elements.number list;
  sum : int;
}

let append_number_to_numbers number_acc numbers position =
  if not (Lib.Collect_number.is_empty number_acc) then
    numbers := Lib.Collect_number.to_number position number_acc :: !numbers

let { sum; _ } =
  Iter.IO.lines_of "input.txt"
  |> Iter.map (fun line ->
         let symbols_positions = ref [] in
         let numbers = ref [] in
         let number_acc = Lib.Collect_number.make () in
         String.iteri
           (fun idx ch ->
             match (is_digit ch, is_dot ch) with
             | false, false ->
                 symbols_positions := idx :: !symbols_positions;
                 append_number_to_numbers number_acc numbers idx
             | _, true -> append_number_to_numbers number_acc numbers idx
             | true, _ -> Lib.Collect_number.append ch number_acc)
           line;
         append_number_to_numbers number_acc numbers (String.length line - 1);
         (!symbols_positions, !numbers |> List.rev))
  |> Iter.foldi
       (fun acc idx line ->
         if idx = 0 then
           {
             acc with
             prev_symbols_positions =
               Lib.Engine_elements.get_symbols_positions line;
             prev_numbers = Lib.Engine_elements.get_numbers line;
           }
         else
           let current_symbols_position =
             Lib.Engine_elements.get_symbols_positions line
           in
           let all_symbols_positions =
             List.append acc.prev_symbols_positions current_symbols_position
           in
           let sum = ref 0 in
           acc.prev_numbers
           |> List.iter (fun (n, r) ->
                  if
                    all_symbols_positions
                    |> List.exists (fun position ->
                           Lib.Range.contains position r)
                  then sum := !sum + n);
           let current_numbers =
             Lib.Engine_elements.get_numbers line
             |> List.filter (fun (n, r) ->
                    if
                      all_symbols_positions
                      |> List.exists (fun position ->
                             Lib.Range.contains position r)
                    then (
                      sum := !sum + n;
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
