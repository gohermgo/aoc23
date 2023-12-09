open Core

let input_data = In_channel.read_lines "input.txt"

let parsed_input =
  let parsed_lines = List.map input_data ~f:(String.split ~on:':') in
  List.nth_exn parsed_lines 1
 
let winning_numbers = List.nth_exn (String.split ~on:'|' parsed_input)

let () = print_endline "Hello, World!"
