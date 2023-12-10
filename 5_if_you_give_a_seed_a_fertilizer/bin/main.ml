open Core

let input_file_data = 
    String.split_lines (In_channel.read_all "input.txt")

let () = List.iteri input_file_data ~f:(
    fun i s -> Stdio.print_endline (String.concat_array [|Int.to_string i; " "; s; "\n---"|])
  )
