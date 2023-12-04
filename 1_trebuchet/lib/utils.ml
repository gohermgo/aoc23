open Base
open Stdio

let log_folder = "logs/"

type log_variant = [ `Input 
  | `Output 
  | `Result 
]

let get_log_name = function
  | `Input -> "input.txt"
  | `Output -> "output.txt"
  | `Result -> "result.txt"

let get_log_path (target: log_variant) : string = 
  String.concat [log_folder; get_log_name target]

let find_bookend_numbers (s: string) : string option =
  let num_string = String.filter s ~f:Char.is_digit in
  let string_length = String.length num_string in
  match string_length with
    | 0 -> None
    | 1 -> let digit_char = String.get num_string 0 in Some(String.of_char_list [digit_char; digit_char])
    | _ -> let (first_num_char, last_num_char) = (String.get num_string 0, String.get num_string (string_length - 1)) in
      Some(String.of_char_list [first_num_char; last_num_char])

let decode_input : string list = let log_path = get_log_path `Input in
  let raw_lines = Stdio.In_channel.read_lines ?fix_win_eol:(Some true) log_path in
  let raw_digit_lines = List.filter_map raw_lines ~f:find_bookend_numbers in
  List.map raw_digit_lines ~f:String.strip

let parse_input : int64 list = List.map decode_input ~f:Int64.of_string

let sum_to_string : string = let sum = List.fold parse_input ~init:Int64.zero ~f:Int64.(+) in Int64.to_string sum

let generate_log : (_) = function
    | `Input -> raise (Invalid_argument "input log is read only")
    | `Output -> let content = decode_input in 
      Stdio.Out_channel.write_lines (get_log_path `Output) content
    | `Result -> let content = sum_to_string in 
      Stdio.Out_channel.write_all (get_log_path `Result) ~data:content
