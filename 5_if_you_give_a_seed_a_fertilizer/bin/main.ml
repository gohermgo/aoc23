open Core

let input_file_data : string = In_channel.read_all "input.txt"

let input_file_lines : string list = String.split_lines input_file_data

let input_file_groups : string list list =
    List.group input_file_lines ~break:(fun _ s -> String.is_empty s)

let rec parse_groups : string list list -> string list = function
    | h :: t ->
        let s = String.concat_lines h in
        let s = String.split s ~on:':' in
        let (_group_name, group_data) = (List.nth_exn s 0, List.nth_exn s 1) in
        (*let whitespace_to_newline = function
            | ' ' -> '\n'
            | c -> c
        in
        let group_data = String.map group_data ~f:whitespace_to_newline in
        let s = String.concat [group_name; ":"; group_data] in*)
        group_data :: parse_groups t  
    | [] -> []

let input_groups : string list = parse_groups input_file_groups
let print_group (gname: string) (gdata: string) : unit =
    Stdio.print_endline (String.concat [gname ;"\n"; gdata])
let seeds_str : string = List.nth_exn input_groups 0
let () = print_group "Seeds" seeds_str

let seeds : int list = List.map (String.split ~on:' ' seeds_str) ~f:Int.of_string

module CustMap =
struct
    type t = { dst_start: int; src_start: int; len: int }
    let aux_range (range_start: int) = 
        (fun i -> i + range_start)

    let dst_range (sm: t) : int list = 
        List.init sm.len ~f:(aux_range sm.dst_start)

    let src_range (sm: t) : int list =
        List.init sm.len ~f:(aux_range sm.src_start)

    let len (sm: t) : int = sm.len

    let of_string (s: string) : t = 
        let fields = String.split s ~on:' ' in
        let fields = List.map fields ~f:Int.of_string in
        let dst_start = List.nth_exn fields 0
        and src_start = List.nth_exn fields 1
        and len = List.nth_exn fields 2
        in { dst_start; src_start; len }

    let of_lines (s: string) : t list =
        let s = String.split_lines s in
        List.map s ~f:of_string
end

let seed_to_soil_str : string = List.nth_exn input_groups 1
let () = print_group "Seed to soil" seed_to_soil_str

let seed_to_soil_map : CustMap.t list = CustMap.of_lines seed_to_soil_str

(*
let seed_to_soil =
    List.iter seed_to_soil_map 
    ~f:(fun map -> 
    )
*)
let soil_to_fert_map : string = List.nth_exn input_groups 2
let () = print_group "Soil to fert" soil_to_fert_map

let fert_to_water_map : string = List.nth_exn input_groups 3
let () = print_group "Fert to water" fert_to_water_map

let water_to_light_map : string = List.nth_exn input_groups 4
let () = print_group "Water to light" water_to_light_map

let light_to_temp_map : string = List.nth_exn input_groups 5
let () = print_group "Light to temperature" light_to_temp_map

let temp_to_humid_map : string = List.nth_exn input_groups 6
let () = print_group "Temperature to humidity" temp_to_humid_map

let humid_to_loc_map : string = List.nth_exn input_groups 7
let () = print_group "Humidity to location" humid_to_loc_map

(*
    Idea: Go through lines recursively until we hit an empty string.
        Gather these strings into a single string again
*)
(*
let rec extract_maps : string list -> string list = function
    (* If string at head empty *)
    | "" :: t -> "" :: extract_maps t
        (*if String.equal h "\n" || String.is_empty h
         
            If the string at head is empty, we return the tail 
        
        then extract_maps t 
        else h :: t
        *)
    | h :: t -> 
        let f = String.substr_replace_all ~pattern:"\n" ~with_:" " in
        let h = f h and

        h :: extract_maps t
    | [] -> []
*)
(*
let () = List.iteri (parse_groups input_file_groups) ~f:(
    fun i s -> Stdio.print_endline (String.concat_array [|Int.to_string i; " "; s; "\n---"|])
  )
*)
