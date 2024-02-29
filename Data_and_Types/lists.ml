1::9::2::8::[];;
[[[]];[[1;2;3]]];;

(* sum the elements of a list*)

let rec sum_list lst =
  match lst with
  |[] -> 0
  |h::t -> h + sum_list t;;

(*it will show the recursive function step by step*)
  #trace sum_list;; 

  (* length of a list*)
let rec length lst =
  match lst with 
  |[]->0
  |h::t-> 1 + length t;;

(* append  two lists in a list*)
let rec append lst1 lst2 =
  match lst1 with 
  |[]-> lst2
  |h::t -> h :: append t lst2;;
(* there is a build in function for append is  "@"*)