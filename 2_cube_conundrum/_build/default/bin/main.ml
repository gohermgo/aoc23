open Stdlib
open Base

let split_whitespace (s: string) : string list = String.split (String.strip s) ~on:' '
let split_comma (p: string) : string list = String.split p ~on:','
let split_semicln (b: string) : string list = String.split b ~on:';'
let split_colon (s: string) : string list = String.split s ~on:':'
let sum_int_list (l: int list) : int = List.fold l ~init:0 ~f:Int.(+)
let int_newline (i: int) : string = String.concat_array [|Int.to_string i; "\n"|]
let print_nicely (d: int list) (o: Out_channel.t) : unit = List.iter (List.map d ~f:int_newline) ~f:(Out_channel.output_string o); Out_channel.close o

let input_file = Stdlib.open_in "input.txt"
let games : string list = In_channel.input_lines input_file
let () = Stdlib.close_in input_file

let color_string_to_array (c: string) : int array = let c_list = split_whitespace c in
  let c_val = Int.of_string (List.nth_exn c_list 0) and c_name = List.nth_exn c_list 1 in match c_name with
    | "red" -> [|c_val; 0; 0|]
    | "green" -> [|0; c_val; 0|]
    | _ -> [|0; 0; c_val|]

let pull_to_arr_list (p: string) : int array list = List.map (split_comma p) ~f:color_string_to_array

(* Part 1 *)
let part_1_id_file = Stdlib.open_out "part_1_ids.txt"
let part_1_sum_file = Stdlib.open_out "part_1_sum.txt"
let array_valid (a: int array) : bool = a.(0) <= 12 && a.(1) <= 13 && a.(2) <= 14

let pull_valid (p: string) : bool = List.for_all (pull_to_arr_list p) ~f:array_valid

let bucket_valid (b: string) : bool = List.for_all (split_semicln b) ~f:pull_valid

let parse_ids (g: string) : int option = let game_info = split_colon g in
  let game_header = split_whitespace (List.nth_exn game_info 0) in
  let game_id = List.nth_exn game_header 1 and game_bucket = List.nth_exn game_info 1 in
  if bucket_valid game_bucket then Some (Int.of_string game_id) else None

let part_1 = let ids = List.filter_map games ~f:parse_ids in print_nicely ids part_1_id_file;
  let id_sum = sum_int_list ids in
  Out_channel.output_string part_1_sum_file (Int.to_string id_sum);
  Out_channel.close part_1_sum_file
(* Part 2 *)
let part_2_power_file = Stdlib.open_out "part_2_powers.txt"
let part_2_sum_file = Stdlib.open_out "part_2_sum.txt"
let bucket_max (in_arr: int array list) : int array = List.fold in_arr ~init:[|0; 0; 0|]
  ~f:(fun i a -> [|Int.max a.(0) i.(0); Int.max a.(1) i.(1); Int.max a.(2) i.(2)|])

let bucket_power (b: string) : int = let values = bucket_max (pull_to_arr_list b) in values.(0) * values.(1) * values.(2)

let parse_powers (g: string) : int = let game_info = split_colon g in
  let game_bucket = String.substr_replace_all (List.nth_exn game_info 1) ~pattern:";" ~with_:"," in
  bucket_power game_bucket

let part_2 = let powers = List.map games ~f:parse_powers in print_nicely powers part_2_power_file;
  let power_sum = sum_int_list powers in
  Out_channel.output_string part_2_sum_file (Int.to_string power_sum);
  Out_channel.close part_1_sum_file

let () = part_1; part_2;
