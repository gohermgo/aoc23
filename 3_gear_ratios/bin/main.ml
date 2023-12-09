open Base
(* Get input *)
let input_file = Stdlib.open_in "input.txt"
let input_data = List.to_array (List.map (In_channel.input_lines input_file) ~f:String.to_array)
let () = Stdlib.close_in input_file
(* Utils *)
let array_slice (arr: 'a array) ~from_:(s_idx: int) ~to_:(e_idx: int) : 'a array =
  Array.filteri arr ~f:(fun i _ -> i >= s_idx && i <= e_idx)
let not_digit c = not (Char.is_digit c)
(*
  match (s_idx, e_idx) with
    | (s, e) when e >= s -> None
    | (s, e) -> let aux i = i >= s && i < e in Some (Array.filteri arr ~f:(fun i _ -> aux i))
*)
let array_remainder (arr: 'a array) ~from_:(idx: int) : 'a array =
  let dim = Array.length arr in
  array_slice arr ~from_:idx ~to_:(dim - 1)
(*
  match array_slice arr ~from_:idx ~to_:(dim - 1) with
    | None -> [| |]
    | Some remainder -> remainder
*)
let get_digits (arr: char array) : char array option = 
  if not_digit arr.(0) then None
  else
  let index_search = Array.findi arr ~f:(fun _ c -> not_digit c) in
  match index_search with
    (* Presumably they are all digits *)
    | None -> Some (arr)
    (* We found some non-digit *)
    | Some (x, _) -> Some (array_slice arr ~from_:0 ~to_:(x - 1))
  
(* 
Part 1
  Any number adjacent to a symbol (not .), even diagonally is to be included
  What are the sum of these numbers?
*)
let search_matrix mat (digit_count: int) ~ri:(ri: int) ~rm:(rm: int) ~ci:(ci: int) ~cm:(cm: int) =
  let dimy = if ri = 0 || ri = rm then 2 else 3 in
  let dimx = if ci = 0 || ci = cm then digit_count + 1 else digit_count + 2 in
  let makex = match ci with
    | 0 -> (fun i -> ci + i)
    | _ -> (fun i -> (ci - 1) + i)
  in
  let x = Array.init dimx ~f:makex in
  let makey = match ri with
    | 0 -> (fun i -> ri + i)
    | _ -> (fun i -> (ri - 1) + i)
  in
  let y  = Array.init dimy ~f:makey in
  Array.map x ~f:(fun i -> Array.map y ~f:(fun j -> mat.(i).(j)))

let indices_of_digits (row: char array) =
  let aux _i c = Char.is_digit c in
  Array.filter_mapi ~f:
  

let check_diagonal_symbols (mat: char array array) =
  let (n_rows, n_cols) = (Array.length mat, Array.length mat.(0)) in
  let (row_bound, col_bound) = (n_rows - 1, n_cols - 1) in
  let row_res = Array.mapi mat ~f:(fun row_idx row ->
    Stdlib.print_endline (String.concat_array [|"Row "; Int.to_string row_idx|]);
    let col_res = Array.filter_mapi row ~f:(fun col_idx _ ->
      let row_remainder = array_remainder row ~from_:col_idx in
      let digit_string = Option.map (get_digits row_remainder) ~f:String.of_array in
      let (digit_count, num) = match digit_string with
        | Some digits -> Stdlib.print_endline digits;
          (String.length digits, Int.of_string_opt digits)
        | None -> (0, None) 
      in
      match num with
        | None -> None
        | Some value -> 
        Stdlib.print_int value;
        Stdlib.print_newline ();
        let search_matrix = search_matrix mat digit_count ~ri:row_idx ~rm:row_bound ~ci:col_idx ~cm:col_bound in
        let f = Array.find search_matrix ~f:(fun search_row -> 
          let f = Array.find search_row ~f:(fun elem -> (not (Char.equal elem '.')) && (not (Char.is_digit elem))) in
          Option.is_some f
        ) in
        if (Option.is_some f) then Some value else None
    ) in
    Array.iter col_res ~f:(fun i -> Stdlib.print_int i; Stdlib.print_newline ());
    col_res
  ) in
  row_res
(*
    for row_idx = 0 to (n_rows - 1) do
    let row = mat.(row_idx) in 
    let col_res = 
      for col_idx=0 to (n_cols - 1) do
      (* Check for digits *)
      let remainder = array_remainder row ~from_:col_idx in
      let digit_string = Option.map (get_digits remainder) ~f:String.of_array in
      (* Get digit count and try to make a number *)
      let (digit_count, num) = match digit_string with
        | Some digits -> (String.length digits, Int.of_string_opt digits)
        | None -> (0, None) 
      in
      (* If we managed to parse the number, here comes the logic *)
      match num with
        | None -> None
          (* 
            Figure out search indices 
            for number of n digits,
              y: row_idx +- 1,
                x: col_idx - 1, ..., col_idx + digit_count
              y: row_idx,
                x: col_idx - 1, col_idx + digit_count
          *)
        | Some x -> 
          let row_bound = (n_rows - 1) in
          let search_row_indices = 
            if row_idx = 0 then [|0; 1|]
            else if row_idx = row_bound then [|row_bound - 1; row_bound |]
            else [|row_idx - 1; row_idx; row_idx + 1|] 
          in
          let col_bound = (n_cols - 1) in
          let sm = Array.map search_row_indices ~f:(fun i ->
            if i = row_idx then
              if col_idx = 0 then [| digit_count |]
              else if col_idx = col_bound then [| col_idx - 1 |]
              else [|col_idx - 1; col_idx + digit_count |]
            else
            if col_idx = 0 then 
              Array.init (digit_count + 1) ~f:(fun i -> col_idx + i)
            else if col_idx = col_bound then 
              Array.init (digit_count + 1) ~f:(fun i -> (col_idx - digit_count) + i)
            else 
              Array.init (digit_count + 2) ~f:(fun i -> col_idx + i)
          ) in
          Array.for_all sm ~f:(fun i -> )
          let search_row_start = if row_idx = 0 
            then row_idx else row_idx - 1
          and search_row_end = if row_idx = (n_rows - 1) 
            then row_idx else row_idx + 1 
          in
          let search_row_indices = Array.init ~f:
          let is_valid = 
            for search_row = search_row_start to search_row_end do
            let search_col_start = if col_idx = 0 
              then col_idx else col_idx - 1
            and search_col_end = if col_idx = (n_cols + digit_count - 1) 
              then col_idx else col_idx + digit_count
            in
              for search_col = search_col_start to search_col_end do
              (* Ignore the number itself *)
              if search_row = row_idx
                then ()
            done
          done in

          
      (* Make it a number *)
      match Option.bind digit_string ~f:Int.of_string_opt with
        (* Here comes the actual checking *)
        | None -> None
        | Some num ->
          
      let elem = row.(col_idx) in
      
      and row_remainder = array_remainder row ~from_:col_idx in
      if Char.is_digit elem then 
        let digit_bound = Array.find row_remainder ~f:(not Char.is_digit) in
      let a = match (x, cocol) with
        |(0, 0) -> (* we are at the first spot, dont check up or left *)
          
        |(0, nn_cols) -> (* we are at column one *)

        |(nnn_rows, 0) ->
        |(dim_x, nnnn_cols) ->
        |_ ->
      in
    done in
  done in
  row_res
*)
let () = 
  let _ = check_diagonal_symbols input_data in
  ()
