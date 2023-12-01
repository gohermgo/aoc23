open Base
open Stdio
(*
let ascii_to_int (c: char) : int = let num_ascii = Char.to_int c and zero_ascii = Char.to_int '0' in
  num_ascii - zero_ascii;;

let ten_64 = Int_conversions.int_to_int64 10

let int64_10powi (exponent: int) : int64 = let exponent_64 = Int_conversions.int_to_int64 exponent in
  Int64.pow ten_64 exponent_64

let string_to_int (s: string) : int64 = let digit_chars = String.to_list s in 
  let digits = List.map digit_chars ~f:Ascii.char_digit_to_int64 
  and aux i elem = let result = Int64.( * ) (int64_10powi i) elem in
    Stdio.print_string (String.concat [Int.to_string i; ":"; Int64.to_string result; ", "]);
    result
  in
  let values = List.rev_mapi digits ~f:aux in
  List.fold values ~init:Int64.zero ~f:Int64.(+);;
*)
let find_bookend_numbers (s: string) : string option =
  let num_string = String.filter s ~f:Char.is_digit in
  let string_length = String.length num_string in
  match string_length with
    | 0 -> None
    | 1 -> let digit_char = String.get num_string 0 in Some(String.of_char_list [digit_char; digit_char; '\n'])
    | _ -> let (first_num_char, last_num_char) = (String.get num_string 0, String.get num_string (string_length - 1)) in
      Some(String.of_char_list [first_num_char; last_num_char])

let () = 
  let input_lines = Stdio.In_channel.read_lines "input.txt" in
  let input_number_strings = List.map (List.filter_map input_lines ~f:find_bookend_numbers) ~f:String.strip in
  Stdio.Out_channel.write_lines "output.txt" input_number_strings;

  let out_channel_calc = Out_channel.create "calc.txt" in
  let aux_parse_input_numbers (num: string) = 
    Stdio.Out_channel.print_string "Making ";
    Stdio.Out_channel.print_string num;
    Stdio.Out_channel.print_endline " an int64";
    Int64.of_string num
  in
  let input_numbers = List.map input_number_strings ~f:aux_parse_input_numbers in
  let aux (a: int64) (b: int64) : int64 = let c = Int64.(+) a b in
    let () = Stdio.print_endline (String.concat ["a: "; Int64.to_string a; "b: "; Int64.to_string b]) in
    let (a_string, b_string, c_string) = (Int64.to_string a, Int64.to_string b, Int64.to_string c) in 
    let math_string = String.concat [a_string; " + "; b_string; " = "; c_string; "\n"] in
    Out_channel.output_string out_channel_calc math_string;
    c 
  in

  let input_sum = List.fold input_numbers ~init:Int64.zero ~f:aux in
  Stdio.Out_channel.write_all "result.txt" ~data:(Int64.to_string input_sum);
  


