type number_pairs = {
  asterisk_position : int;
  first : int option;
  second : int option;
  count_to_life : int;
}

type acc = {
  sum : int;
  pairs_is_progress : number_pairs list;
  prev_numbers : Lib.Collect_number.t list;
}

let make_number_pairs asterisk_position =
  { first = None; second = None; asterisk_position; count_to_life = 3 }

let reduce_life pairs =
  {
    pairs with
    count_to_life =
      (if pairs.count_to_life <= 0 then 0 else pairs.count_to_life - 1);
  }

let has_both_numbers = function
  | { first = Some _; second = Some _; _ } -> true
  | _ -> false

let is_dead = function { count_to_life = 0; _ } -> true | _ -> false

let print_error_state { first; second; _ } =
  let open Format in
  asprintf "invalid state of number_pairs: first is %a, second is %a"
    (pp_print_option pp_print_int)
    first
    (pp_print_option pp_print_int)
    second

let add number = function
  | { first = None; _ } as t -> { t with first = Some number }
  | { first = Some _; second = None; _ } as t -> { t with second = Some number }
  | t -> raise (Invalid_argument (print_error_state t))

let multiply = function
  | { first = Some a; second = Some b; _ } -> a * b
  | t -> raise (Invalid_argument (print_error_state t))

let { sum; _ } =
  Iter.IO.lines_of "input.txt"
  |> Iter.map (fun line ->
         let asterisk_positions, numbers_with_properties =
           Lib.Collect_number.parse_line line
         in
         (asterisk_positions, numbers_with_properties))
  |> Iter.fold
       (fun acc (asterisk_position, numbers_with_properties) ->
         let current_numbers = ref numbers_with_properties in
         let found_pairs, pairs_is_progress =
           asterisk_position |> List.map make_number_pairs
           |> List.append acc.pairs_is_progress
           |> List.map reduce_life
           |> List.filter (fun el -> not (is_dead el))
           |> List.map (fun pair ->
                  let pair_ref = ref pair in
                  acc.prev_numbers
                  |> List.iter (fun number ->
                         if
                           Lib.Range.contains !pair_ref.asterisk_position
                             (Lib.Collect_number.get_range number)
                         then
                           pair_ref :=
                             add
                               (Lib.Collect_number.get_number number)
                               !pair_ref);
                  current_numbers :=
                    !current_numbers
                    |> List.filter (fun number ->
                           if
                             Lib.Range.contains !pair_ref.asterisk_position
                               (Lib.Collect_number.get_range number)
                           then (
                             pair_ref :=
                               add
                                 (Lib.Collect_number.get_number number)
                                 !pair_ref;
                             false)
                           else true);
                  !pair_ref)
           |> List.partition has_both_numbers
         in
         {
           sum =
             acc.sum
             + List.fold_left (fun acc el -> acc + multiply el) 0 found_pairs;
           pairs_is_progress;
           prev_numbers = !current_numbers;
         })
       { sum = 0; pairs_is_progress = []; prev_numbers = [] }

let _ = Format.printf "Sum is %i\n" sum
