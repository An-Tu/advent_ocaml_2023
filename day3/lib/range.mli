type t

val ( -- ) : int -> int -> t
val start : t -> int
val end_ : t -> int
val contains : int -> t -> bool
val contains_or_is_contained : t -> t -> bool
val overlaps_or_is_overlapped : t -> t -> bool
