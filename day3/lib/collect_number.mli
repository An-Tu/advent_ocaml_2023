type t

val get_number : t -> int
val get_range : t -> Range.t
val parse_line : string -> int list * t list
