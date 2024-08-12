type t = char list ref

let make () = ref []
let append ch t = t := ch :: !t
let is_empty t = List.length !t = 0

let to_number position t =
  let number_len = List.length !t in
  let number = !t |> Iter.of_list |> Iter.rev |> Iter.to_str |> int_of_string in
  t := [];
  Engine_elements.make_number number position number_len
