type number = int * Range.t
type t = int list * number list

let make_number n p num_len = (n, Range.(p - num_len - 1 -- p))
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
         Format.fprintf fmt "N: (%i) R: %i -- %i" n (Range.start r)
           (Range.end_ r)))
    numbers
