type t = { winning_numbers : int list; our_numbers : int list }

let is_digit = function '0' .. '9' -> true | _ -> false
let is_space = function ' ' -> true | _ -> false

let one_or_two_digit_number =
  let open Angstrom in
  let* first_ch = any_char in
  let+ second_ch = any_char in
  match (is_digit first_ch, is_digit second_ch) with
  | true, true ->
      String.init 2 (function
        | 0 -> first_ch
        | 1 -> second_ch
        | _ -> failwith "Not expected string index")
      |> int_of_string
  | false, true -> second_ch |> Char.escaped |> int_of_string
  | _ ->
      raise
        (Invalid_argument
           (Format.sprintf "Invalid chars in number. First: %c, Second: %c"
              first_ch second_ch))

let space =
  let open Angstrom in
  let+ _ = char ' ' in
  ()

let divider =
  let open Angstrom in
  let+ _ = char '|' in
  ()

let card_begin =
  let open Angstrom in
  let* _ = string "Card" in
  let* _ = take_while1 is_space in
  let* _ = take_while1 is_digit in
  let+ _ = string ": " in
  ()

let number_with_space =
  let open Angstrom in
  one_or_two_digit_number <* space

let divider_with_space =
  let open Angstrom in
  divider <* space

let line =
  let open Angstrom in
  let* _ = card_begin in
  let* winning_numbers = many_till number_with_space divider_with_space in
  let+ our_numbers =
    many_till (number_with_space <|> one_or_two_digit_number) end_of_line
  in
  { winning_numbers; our_numbers }

let parse_all =
  let open Angstrom in
  many1 line
