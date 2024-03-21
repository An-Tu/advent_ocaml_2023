let[@inline] batching_iter fn seq k = fn seq k

let str_to_digit = function
  | "0" -> 0
  | "1" | "one" -> 1
  | "2" | "two" -> 2
  | "3" | "three" -> 3
  | "4" | "four" -> 4
  | "5" | "five" -> 5
  | "6" | "six" -> 6
  | "7" | "seven" -> 7
  | "8" | "eight" -> 8
  | "9" | "nine" -> 9
  | str -> failwith ("String must be some digit. Got: " ^ str)

let digits_reg =
  Str.regexp {|[0-9]\|one\|two\|three\|four\|five\|six\|seven\|eight\|nine|}

let res =
  Iter.IO.lines_of "input.txt"
  |> batching_iter (fun it next ->
         it (fun line ->
             let first =
               let _ = Str.search_forward digits_reg line 0 in
               Str.matched_string line
             in
             let last =
               let _ =
                 Str.search_backward digits_reg line (String.length line)
               in
               Str.matched_string line
             in
             let num = (str_to_digit first * 10) + str_to_digit last in
             print_int num;
             print_newline ();
             next num))
  |> Iter.sum
;;

print_endline ("Result is " ^ string_of_int res)
