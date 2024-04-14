let rec n_time (f,n,x)= 
if n = 0 then x else f (n_time(f , n-1 , x));;
(* val n_time : ('a -> 'a) * int * 'a -> 'a = <fun> *)
let rec sum1 acc = function
|[] -> acc
|h::t -> sum1 (h+acc) t;;
(* val sum1 : int -> int list -> int = <fun> *)
(* let sum = sun1 0;; *)
(* Error: Unbound value sun1
Hint: Did you mean sum1? *)
let sum = sum1 0;;
(* val sum : int list -> int = <fun> *)
sum [1;2;3;4;5;6;7];;
(* - : int = 28 *)
let add_lists l1 l2 = List.map2 (fun x y -> x+y) l1 l2;;
(* val add_lists : int list -> int list -> int list = <fun> *)
let add_matrices m1 m2 = List.map2 (fun x y -> add_lists x y) m1 m2;;
(* val add_matrices : int list list -> int list list -> int list list =
  <fun> *)
add_matrices [[1;2];[3;4]] [[3;4];[2;1]];;
(* - : int list list = [[4; 6]; [5; 5]] *)
let add_lists l1 l2 = if List.length l1<>List.length l2 then failwith "invalid for addition" else List.map2 (fun x y -> x+y) l1 l2;;
(* val add_lists : int list -> int list -> int list = <fun> *)
let add_matrices m1 m2 =if List.length m1<>List.length m2 then failwith "invalid argument" else List.map2 (fun x y -> add_lists x y) m1 m2;;
(* val add_matrices : int list list -> int list list -> int list list =
  <fun> *)
add_matrices [[1;2];[3;4;3]] [[3;4];[2;1]];;
(* Exception: Failure "invalid for addition". *)
add_matrices [[1;2]] [[3;4];[2;1]];;
(* Exception: Failure "invalid argument". *)
let rec next lst =
  match lst with
  | [] -> [1]
  | [a] -> [a; a]
  | hd1 :: hd2 :: tl ->
    let rec compute_next acc lst =
      match lst with
      | [] -> List.rev acc
      | [x] -> List.rev (x :: acc)
      | hd1 :: hd2 :: tl ->
        let sum = hd1 + hd2 in
        compute_next (sum :: acc) (hd2 :: tl)
    in
    hd1 :: compute_next [hd1 + hd2] (hd2 :: tl)
;;
(* val next : int list -> int list = <fun> *)
let () =
  let test_cases = [[1]; [1; 2; 3]; [1; 2; 3; 4]; [1; 2; 3; 4; 5]] in
  List.iter (fun lst -> 
    let result = next lst in
    Printf.printf "Input: [%s] -> Output: [%s]\n"
      (String.concat "; " (List.map string_of_int lst))
      (String.concat "; " (List.map string_of_int result))
  ) test_cases
;;
