type string_or_int =
  | String of string
  | Int of int

type string_or_int_list = string_or_int list

let rec sum : string_or_int list -> int = function
  | [] -> 0
  | String s :: t -> int_of_string s + sum t
  | Int i :: t -> i + sum t

let lst_sum = sum [String "1"; Int 2]

(*code of odd and even*)
let rec even n =
match n with 
|0->true 
|n -> odd(n-1)
and odd n=
match n with 
|0->false 
|n -> even(n-1)
