let[@inline] batching_iter fn seq k = fn seq k
let is_digit = function '0' .. '9' -> true | _ -> false
let from_char_to_digit c = int_of_char c - int_of_char '0'

let res =
  Iter.IO.lines_of "input.txt"
  |> batching_iter (fun it next ->
         it (fun line ->
             let acc =
               line |> String.to_seq
               |> Seq.fold_left
                    (fun acc char ->
                      if is_digit char then
                        match acc with
                        | None, None -> (Some char, None)
                        | Some c, None | Some c, Some _ -> (Some c, Some char)
                        | _ -> failwith "Invalid acc"
                      else acc)
                    (None, None)
             in
             let num =
               match acc with
               | Some a, Some b ->
                   (from_char_to_digit a * 10) + from_char_to_digit b
               | Some a, None ->
                   (from_char_to_digit a * 10) + from_char_to_digit a
               | _ ->
                   failwith
                     ("Must be at least one digit in the line. Line: " ^ line)
             in
             print_int num;
             print_newline ();
             next num))
  |> Iter.sum
;;

print_endline ("Result is " ^ string_of_int res)
