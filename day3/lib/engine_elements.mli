type number = int * Range.t
type t = int list * number list

val make_number : int -> int -> int -> number
val get_symbols_positions : t -> int list
val get_numbers : t -> number list
val pp : Format.formatter -> t -> unit
