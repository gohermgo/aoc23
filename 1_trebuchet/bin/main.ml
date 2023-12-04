open Base
open Stdio
open Trebuchet_lib
(*
let find_bookend_numbers (s: string) : string option =
  let num_string = String.filter s ~f:Char.is_digit in
  let string_length = String.length num_string in
  match string_length with
    | 0 -> None
    | 1 -> let digit_char = String.get num_string 0 in Some(String.of_char_list [digit_char; digit_char])
    | _ -> let (first_num_char, last_num_char) = (String.get num_string 0, String.get num_string (string_length - 1)) in
      Some(String.of_char_list [first_num_char; last_num_char])
*)
let () = 
  (*let input_lines = Stdio.In_channel.read_lines "input.txt" in*)
  let input_number_strings = List.map (List.filter_map input_lines ~f:Utils.find_bookend_numbers) ~f:String.strip in
  Stdio.Out_channel.write_lines "output.txt" input_number_strings;

  let input_numbers = List.map input_number_strings ~f:Int64.of_string in
  let input_sum = List.fold input_numbers ~init:Int64.zero ~f:Int64.(+) in
  Stdio.Out_channel.write_all "result.txt" ~data:(Int64.to_string input_sum);
