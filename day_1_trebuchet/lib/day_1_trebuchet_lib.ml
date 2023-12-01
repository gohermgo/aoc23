open Base

(*
module Ascii :
  sig
    val to_64 : char -> int64
    val zero_64 : int64
    val to_int_64: char -> int64
  end
=
struct
  let to_64 (c: char) : int64 = Int_conversions.int_to_int64 (Char.to_int c)

  let zero_64 = to_64 '0'

  let to_int_64 (c: char) : int64 = let ascii_num_64 = to_64 c in
    Int64.(-) ascii_num_64 zero_64
end;;
*)
