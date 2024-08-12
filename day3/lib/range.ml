type t = int * int

let ( -- ) from to_ = (from, to_)
let start (from, _) = from
let end_ (_, to_) = to_
let contains n (from, to_) = from <= n && to_ >= n

let overlaps other self =
  self |> contains (other |> start) || self |> contains (other |> end_)

let contains_range other self =
  self |> contains (other |> start) && self |> contains (other |> end_)

let contains_or_is_contained r1 r2 =
  r1 |> contains_range r2 || r2 |> contains_range r1

let overlaps_or_is_overlapped r1 r2 = r1 |> overlaps r2 || r2 |> overlaps r1
