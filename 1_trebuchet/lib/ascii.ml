open Base

module Ascii :
  sig
    val char_to_int64 : char -> int64
    (*val zero_64 : int64*)
    val char_digit_to_int64: char -> int64
  end
=
struct
  let char_to_int64 (c: char) : int64 = Int_conversions.int_to_int64 (Char.to_int c)
  let zero_64 = char_to_int64 '0'
  let char_digit_to_int64 (c: char) : int64 = let ascii_num_64 = char_to_int64 c in
    Int64.(-) ascii_num_64 zero_64
end;;

let test_aux_char_primal (value: int) (digit: char) (fut: char -> int64) = 
  let expected = Int_conversions.int_to_int64 value
  and actual = fut digit in
  equal_int64 expected actual

let test_aux_char_to_int64 (ascii: int) (digit: char) = test_aux_char_primal ascii digit Ascii.char_to_int64

let%test "0: Char to int 64" = test_aux_char_to_int64 48 '0'
let%test "1: Char to int 64" = test_aux_char_to_int64 49 '1'
let%test "2: Char to int 64" = test_aux_char_to_int64 50 '2'
let%test "3: Char to int 64" = test_aux_char_to_int64 51 '3'
let%test "4: Char to int 64" = test_aux_char_to_int64 52 '4'
let%test "5: Char to int 64" = test_aux_char_to_int64 53 '5'
let%test "6: Char to int 64" = test_aux_char_to_int64 54 '6'
let%test "7: Char to int 64" = test_aux_char_to_int64 55 '7'
let%test "8: Char to int 64" = test_aux_char_to_int64 56 '8'
let%test "9: Char to int 64" = test_aux_char_to_int64 57 '9'

let test_aux_char_digit_to_int64 (number: int) (digit: char) = test_aux_char_primal number digit Ascii.char_digit_to_int64

let%test "0: Char digit to int 64" = test_aux_char_digit_to_int64 0 '0'
let%test "1: Char digit to int 64" = test_aux_char_digit_to_int64 1 '1'
let%test "2: Char digit to int 64" = test_aux_char_digit_to_int64 2 '2'
let%test "3: Char digit to int 64" = test_aux_char_digit_to_int64 3 '3'
let%test "4: Char digit to int 64" = test_aux_char_digit_to_int64 4 '4'
let%test "5: Char digit to int 64" = test_aux_char_digit_to_int64 5 '5'
let%test "6: Char digit to int 64" = test_aux_char_digit_to_int64 6 '6'
let%test "7: Char digit to int 64" = test_aux_char_digit_to_int64 7 '7'
let%test "8: Char digit to int 64" = test_aux_char_digit_to_int64 8 '8'
let%test "9: Char digit to int 64" = test_aux_char_digit_to_int64 9 '9'
