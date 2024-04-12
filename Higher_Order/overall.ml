let double x = 2 * x
let square x = x * x;;
(* val double : int -> int = <fun>
val square : int -> int = <fun> *)
let quad x = double (double x)
let fourth x = square (square x);;
(* val quad : int -> int = <fun>
val fourth : int -> int = <fun> *)
let twice f x = f (f x);;
(* val twice : ('a -> 'a) -> 'a -> 'a = <fun> *)
twice square 2;;
(* - : int = 16 *)
let pipeline x f = f x
let (|>) = pipeline
let x = 5 |> double;;
(* val pipeline : 'a -> ('a -> 'b) -> 'b = <fun>
val ( |> ) : 'a -> ('a -> 'b) -> 'b = <fun>
val x : int = 10 *)
(** [add1 lst] adds 1 to each element of [lst] *)
let rec add1 = function
  | [] -> []
  | h :: t -> (h + 1) :: add1 t

let lst1 = add1 [1; 2; 3];;
(* val add1 : int list -> int list = <fun>
val lst1 : int list = [2; 3; 4] *)
let rec add_aa = function
|[]->[]
|h::t->(h^"aa")::add_aa t;;
(* val add_aa : string list -> string list = <fun> *)
add_aa ["hi";"solo"];;
(* - : string list = ["hiaa"; "soloaa"] *)
List.mapi;;
(* - : (int -> 'a -> 'b) -> 'a list -> 'b list = <fun> *)
List.map;;
(* - : ('a -> 'b) -> 'a list -> 'b list = <fun> *)
List.map2;;
(* - : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list =
<fun> *)
List.map (fun x -> x+10) [2;3;4;5;56];;
(* - : int list = [12; 13; 14; 15; 66] *)
let rec mapp f acc = function 
|[]-> acc 
|h::t-> mapp f (acc@(f h)) t;;
(* val mapp :
  ('a -> 'b list) -> 'b list -> 'a list -> 'b list =
  <fun> *)
let rec mapp f acc = function 
|[]-> acc 
|h::t-> mapp f (acc@[f h]) t;;
(* val mapp : ('a -> 'b) -> 'b list -> 'a list -> 'b list =
  <fun> *)
let map_tr f = mapp f [] ;;
(* val map_tr : ('a -> 'b) -> 'a list -> 'b list = <fun> *)
(** [even n] is whether [n] is even. *)
let even n =
  n mod 2 = 0

(** [evens lst] is the sublist of [lst] containing only even numbers. *)
let rec evens = function
  | [] -> []
  | h :: t -> if even h then h :: evens t else evens t

let lst1 = evens [1; 2; 3; 4];;
(* val even : int -> bool = <fun>
val evens : int list -> int list = <fun>
val lst1 : int list = [2; 4] *)
(** [odd n] is whether [n] is odd. *)
let odd n =
  n mod 2 <> 0

(** [odds lst] is the sublist of [lst] containing only odd numbers. *)
let rec odds = function
  | [] -> []
  | h :: t -> if odd h then h :: odds t else odds t

let lst2 = odds [1; 2; 3; 4];;
(* val odd : int -> bool = <fun>
val odds : int list -> int list = <fun>
val lst2 : int list = [1; 3] *)
let rec filter p = function
  | [] -> []
  | h :: t -> if p h then h :: filter p t else filter p t;;
(* val filter : ('a -> bool) -> 'a list -> 'a list = <fun> *)
filter (fun s-> s =5) [1;2;3;4;5;6];;
(* - : int list = [5] *)
let rec combine' f lst acc = match lst with
  | [] -> acc
  | h :: t -> f h (combine' f t acc)

let sum lst = combine' ( + ) lst 0
let concat lst = combine' ( ^ ) lst "";;
(* val combine' : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b =
  <fun>
val sum : int list -> int = <fun>
val concat : string list -> string = <fun> *)
let rec fold_right f lst acc = match lst with
  | [] -> acc
  | h :: t -> f h (fold_right f t acc);;
(* val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b =
  <fun> *)
let rec fold_left f acc = function
  | [] -> acc
  | h :: t -> fold_left f (f acc h) t

let sum = fold_left ( + ) 0
let concat = fold_left ( ^ ) "";;
(* val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a =
  <fun>
val sum : int list -> int = <fun>
val concat : string list -> string = <fun> *)
