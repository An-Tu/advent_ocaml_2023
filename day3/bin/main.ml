module R = Lib.Range.Range

module Engine_Elements = struct
  type position = int
  type number = int * R.t
  type t = position list * number list

  let make_number n p num_len = (n, R.(p - num_len - 1 -- p))
  let get_symbols_positions (positions, _) = positions
  let get_numbers (_, numbers) = numbers

  let pp fmt (symbols_positions, numbers) =
    Format.fprintf fmt "Symbols positions: %a\nNumbers: %a"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "%s" "|")
         Format.pp_print_int)
      symbols_positions
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "%s" "")
         (fun fmt (n, r) ->
           Format.fprintf fmt "N: (%i) R: %i -- %i" n (R.start r) (R.end_ r)))
      numbers
end

module Collect_Number : sig
  type t

  val make : unit -> t
  val append : char -> t -> unit
  val is_empty : t -> bool
  val to_number : Engine_Elements.position -> t -> Engine_Elements.number
end = struct
  type t = char list ref

  let make () = ref []
  let append ch t = t := ch :: !t
  let is_empty t = List.length !t = 0

  let to_number position t =
    let number_len = List.length !t in
    let number =
      !t |> List.rev |> Iter.of_list |> Iter.to_str |> int_of_string
    in
    t := [];
    Engine_Elements.make_number number position number_len
end

let is_digit = function '0' .. '9' -> true | _ -> false
let is_dot = function '.' -> true | _ -> false

type acc = {
  prev_symbols_positions : Engine_Elements.position list;
  prev_numbers : Engine_Elements.number list;
  sum : int;
}

let append_number_to_numbers number_acc numbers p =
  if not (Collect_Number.is_empty number_acc) then
    numbers := Collect_Number.to_number p number_acc :: !numbers

let { sum; _ } =
  Iter.IO.lines_of "input.txt"
  |> Iter.map (fun line ->
         let symbols = ref [] in
         let numbers = ref [] in
         let number_acc = Collect_Number.make () in
         String.iteri
           (fun p ch ->
             match (is_digit ch, is_dot ch) with
             | false, false ->
                 symbols := p :: !symbols;
                 append_number_to_numbers number_acc numbers p
             | _, true -> append_number_to_numbers number_acc numbers p
             | true, _ -> Collect_Number.append ch number_acc)
           line;
         append_number_to_numbers number_acc numbers (String.length line - 1);
         (!symbols, !numbers |> List.rev))
  |> Iter.foldi
       (fun acc idx line ->
         if idx = 0 then
           {
             acc with
             prev_symbols_positions = Engine_Elements.get_symbols_positions line;
             prev_numbers = Engine_Elements.get_numbers line;
           }
         else
           let current_symbols_position =
             Engine_Elements.get_symbols_positions line
           in
           let all_symbols_positions =
             List.append acc.prev_symbols_positions current_symbols_position
           in
           let sum = ref 0 in
           acc.prev_numbers
           |> List.iter (fun (n, r) ->
                  if
                    all_symbols_positions
                    |> List.exists (fun position -> R.contains position r)
                  then sum := !sum + n);
           let current_numbers =
             Engine_Elements.get_numbers line
             |> List.filter (fun (n, r) ->
                    if
                      all_symbols_positions
                      |> List.exists (fun position -> R.contains position r)
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
