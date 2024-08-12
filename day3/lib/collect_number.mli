type t

val make : unit -> t
val append : char -> t -> unit
val is_empty : t -> bool
val to_number : int -> t -> Engine_elements.number
