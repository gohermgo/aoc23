(*open Base
open Day_1_trebuchet_lib.Ascii
module String :
  sig
    type t = string
    val make : int -> char -> t
    val init : int -> (int -> char) -> t
    val empty : t
    external length : t -> int = "%string_length"
    external get : t -> int -> char = "%string_safe_get"
    val of_bytes : bytes -> t
    val to_bytes : t -> bytes
    val blit : t -> int -> bytes -> int -> int -> unit
    val concat : t -> t list -> t
    val cat : t -> t -> t
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val starts_with : prefix:t -> t -> bool
    val ends_with : suffix:t -> t -> bool
    val contains_from : t -> int -> char -> bool
    val rcontains_from : t -> int -> char -> bool
    val contains : t -> char -> bool
    val sub : t -> int -> int -> t
    val split_on_char : char -> t -> t list
    val map : (char -> char) -> t -> t
    val mapi : (int -> char -> char) -> t -> t
    val fold_left : ('acc -> char -> 'acc) -> 'acc -> t -> 'acc
    val fold_right : (char -> 'acc -> 'acc) -> t -> 'acc -> 'acc
    val for_all : (char -> bool) -> t -> bool
    val exists : (char -> bool) -> t -> bool
    val trim : t -> t
    val escaped : t -> t
    val uppercase_ascii : t -> t
    val lowercase_ascii : t -> t
    val capitalize_ascii : t -> t
    val uncapitalize_ascii : t -> t
    val iter : (char -> unit) -> t -> unit
    val iteri : (int -> char -> unit) -> t -> unit
    val index_from : t -> int -> char -> int
    val index_from_opt : t -> int -> char -> int option
    val rindex_from : t -> int -> char -> int
    val rindex_from_opt : t -> int -> char -> int option
    val index : t -> char -> int
    val index_opt : t -> char -> int option
    val rindex : t -> char -> int
    val rindex_opt : t -> char -> int option
    val is_valid_utf_8 : t -> bool
    val is_valid_utf_16be : t -> bool
    val is_valid_utf_16le : t -> bool
    val get_uint8 : t -> int -> int
    val get_int8 : t -> int -> int
    val get_uint16_ne : t -> int -> int
    val get_uint16_be : t -> int -> int
    val get_uint16_le : t -> int -> int
    val get_int16_ne : t -> int -> int
    val get_int16_be : t -> int -> int
    val get_int16_le : t -> int -> int
    val get_int32_ne : t -> int -> int32
    val hash : t -> int
    val seeded_hash : int -> t -> int
    val get_int32_be : t -> int -> int32
    val get_int32_le : t -> int -> int32
    val get_int64_ne : t -> int -> int64
    val get_int64_be : t -> int -> int64
    val get_int64_le : t -> int -> int64
    external unsafe_get : t -> int -> char = "%string_unsafe_get"
    external unsafe_blit : t -> int -> bytes -> int -> int -> unit
      = "caml_blit_string" [@@noalloc]
    val to_int64: t -> int64
  end
= 
struct
  include String
  let to_int64 (s: string) : int64 = let digits = String.to_list s in
    let digits = List.map digits ~f:Ascii.to_int_64
    and base = Int_conversions.int_to_int64 10 in
    let aux i = Int64.pow base (Int_conversions.int_to_int64 i) in
    let aux i elem = Int64.( * ) (aux i) elem in
    let values = List.rev_mapi digits ~f:aux in
    List.fold values ~init:Int64.zero ~f:Int64.(+)
end;;
*)
