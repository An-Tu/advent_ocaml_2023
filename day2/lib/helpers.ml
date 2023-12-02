type cube = Red of int | Green of int | Blue of int

let to_string = function
  | Red num -> "Red count: " ^ string_of_int num
  | Green num -> "Green count: " ^ string_of_int num
  | Blue num -> "Blue count: " ^ string_of_int num

type game = { game_id : int; subsets : cube list list }

let is_digit = function '0' .. '9' -> true | _ -> false

let game =
  let open Angstrom in
  string "Game " *> (take_while1 is_digit >>| int_of_string) <* string ": "

let red =
  let open Angstrom in
  take_while1 is_digit <* string " red" >>= fun num ->
  return (Red (int_of_string num))

let green =
  let open Angstrom in
  take_while1 is_digit <* string " green" >>= fun num ->
  return (Green (int_of_string num))

let blue =
  let open Angstrom in
  take_while1 is_digit <* string " blue" >>= fun num ->
  return (Blue (int_of_string num))

let cube =
  let open Angstrom in
  red <|> green <|> blue

let subset =
  let open Angstrom in
  many1
    ( cube >>= fun c ->
      peek_char >>= fun char ->
      match char with
      | None | Some ';' -> return c
      | _ -> string ", " >>= fun _ -> return c )

let subsets =
  let open Angstrom in
  many1
    ( subset >>= fun sub ->
      peek_char >>= fun char ->
      match char with
      | Some ';' -> string "; " >>= fun _ -> return sub
      | None -> return sub
      | _ -> fail "invalid string" )

let game_line =
  let open Angstrom in
  game >>= fun game_id ->
  subsets
  >>= (fun subsets -> return { game_id; subsets })
  <* (end_of_line <|> end_of_input)
