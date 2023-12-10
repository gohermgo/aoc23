open Core

(* The lines as a list *)
let input_lines = In_channel.read_lines "input.txt"

let get_exn_list_i i = fun line -> List.nth_exn line i
(*
  Part 1
    Sum of points where card is
    [winning] | [actual]
    and points is number actual in set of winning
*)
(* The input data itself as a list *)
let input_data_lines = 
  (* Data x : [data] *)
  let split_lines = List.map input_lines ~f:(String.split ~on:':') in
  (* Get data *)
  List.map split_lines ~f:(get_exn_list_i 1)

let input_data_split = List.map input_data_lines ~f:(String.split ~on:'|')

let intify_strings (list: string list) : int list = List.filter_map list ~f:Int.of_string_opt

let fetch_set (line_set: string list) =
  let split_set = List.map line_set ~f:(String.split ~on:' ') in
  List.map split_set ~f:intify_strings

let winning_set = fetch_set (List.map input_data_split ~f:(get_exn_list_i 0))

let actual_set = fetch_set (List.map input_data_split ~f:(get_exn_list_i 1))

let exists_in_set (set: int list) = fun i -> List.exists set ~f:(fun j -> j = i)

let score_card (win: int list) (pick: int list) : int =
  let count_in_winning = List.count pick ~f:(exists_in_set win) in
  (*if count_in_winning > 1 then
    (2 * (count_in_winning - 1)) + 1
  else count_in_winning*)
  if count_in_winning > 1 then
  Int.pow 2 (count_in_winning - 1)
  else count_in_winning

let part_1 =
  (*let score_list = List.map2 winning_set actual_set ~f:(
    fun win pick -> List.count pick ~f:(
      fun pick_num -> List.exists win ~f:(
        fun win_num -> pick_num = win_num
      )
    )
  ) in*)
  let score_list = List.map2 winning_set actual_set ~f:score_card in
  match score_list with
    | Base.List.Or_unequal_lengths.Unequal_lengths -> 0
    | Ok l -> List.fold l ~init:0 ~f:Int.(+)

let () = Stdio.print_endline (Int.to_string part_1)
