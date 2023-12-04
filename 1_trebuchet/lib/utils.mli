type log_variant  = [ `Input 
  | `Output 
  | `Result 
]

val get_log_name : log_variant -> string

val get_log_name : log_variant -> string

val find_bookend_numbers : string -> string option

val decode_input : string list

val sum_to_string : string

val parse_input : int64 list
