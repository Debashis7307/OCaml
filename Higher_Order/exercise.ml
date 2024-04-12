let double x = 2*x
let square x = x*x
let twice f x = f (f x)
let quad = twice double
let fourth = twice square;;
(* val double : int -> int = <fun>
val square : int -> int = <fun>
val twice : ('a -> 'a) -> 'a -> 'a = <fun>
val quad : int -> int = <fun>
val fourth : int -> int = <fun> *)
let ( @@ ) f g x = x |> g |> f;;
(* val ( @@ ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b = <fun> *)
(* String.length @@ string_of_int 1;; *)
(* Error: This expression has type string but an expression was expected of type
         'a -> string *)
(* @@ String.length string_of_int 1;; *)
(* Error: Syntax error *)
String.length @@ string_of_int ;;
(* - : int -> int = <fun> *)
( @@ ) String.length string_of_int 1;;
(* - : int = 1 *)
( @@ ) String.length string_of_int 100;;
(* - : int = 3 *)
let twice f x = f(f x);;
(* val twice : ('a -> 'a) -> 'a -> 'a = <fun> *)
twice (fun x -> x*2) 5;;
(* - : int = 20 *)
(* let rec repeat f n x = 
if n = 0 then f x else repeat f (n-1) f x;; *)
(* Error: This expression has type 'a -> 'b -> 'c
       but an expression was expected of type 'a
       The type variable 'a occurs inside 'a -> 'b -> 'c *)
(* let rec repeat f n x = 
if n = 0 then  x else repeat f (n-1) f x;; *)
(* Error: This expression has type 'a -> 'b but an expression was expected of type
         'a
       The type variable 'a occurs inside 'a -> 'b *)
let rec repeat f n x = 
if n = 0 then  x else repeat f (n-1) (f x);;
(* val repeat : ('a -> 'a) -> int -> 'a -> 'a = <fun> *)
(* repeat (+) 5 1;; *)
(* Error: This expression has type int -> int -> int
       but an expression was expected of type int -> int
       Type int -> int is not compatible with type int *)
repeat (fun x -> x+1) 5 1;;
(* - : int = 6 *)
let product_left lst = List.fold_left (fun x acc->acc*.x) 1.0 lst;;
(* val product_left : float list -> float = <fun> *)
let x = 9;;
(* val x : int = 9 *)
let product_left lst = List.fold_left ( *. ) 1.0 lst ;;
(* val product_left : float list -> float = <fun> *)
let product_right lst = List.fold_right ( *. ) lst 1.0;;
(* val product_right : float list -> float = <fun> *)
(* product_right [1;2;3;4;6;6];; *)
(* Error: This expression has type int but an expression was expected of type
         float
  Hint: Did you mean `1.'? *)
product_right [1.;2.];;
(* - : float = 2. *)
product_right [1.;79.];;
(* - : float = 79. *)
let sum_cube_odd n =
  let odd_numbers = List.filter (fun x -> x mod 2 <> 0) (List.init (n + 1) (fun i -> i)) in
  List.fold_left (fun acc x -> acc + (x * x * x)) 0 odd_numbers;;
(* val sum_cube_odd : int -> int = <fun> *)
sum_cube_odd 5;;
(* - : int = 153 *)
(* let make_lst n = List.init (fun x -> x ) n;; *)
(* Error: This expression should not be a function, the expected type is int *)
let make_lst n = List.init n (fun x -> x ) ;;
(* val make_lst : int -> int list = <fun> *)
make_lst 5;;
(* - : int list = [0; 1; 2; 3; 4] *)
(* let make_lst n = List.init n+1 (fun x -> x ) ;; *)
(* Error: This expression has type (int -> 'a) -> 'a list
       but an expression was expected of type int *)
let make_lst n = List.init (n+1) (fun x -> x ) ;;
(* val make_lst : int -> int list = <fun> *)
make_lst 5;;
(* - : int list = [0; 1; 2; 3; 4; 5] *)
(* let sum_cube_odd_pipe n = n |> make_lst |> (fun x -> x mod 2 <> 0 ) |> List.filter |> (fun x -> x*x*x) |>  List.map |> 0 |> ( + ) |> List.fold_left;; *)
(* Error: This expression has type int list but an expression was expected of type
         int *)
(* let sum_cube_odd_pipe n = n |> make_lst |> List.filter (fun x -> x mod 2 <> 0 ) |> List.map (fun x -> x*x*x) |> List.fold_left 0 ( + );; *)
(* Error: This expression has type int but an expression was expected of type
         'a -> 'b -> 'a *)
let sum_cube_odd_pipe n = n |> make_lst |> List.filter (fun x -> x mod 2 <> 0 ) |> List.map (fun x -> x*x*x) |> List.fold_left ( + ) 0 ;;
(* val sum_cube_odd_pipe : int -> int = <fun> *)
sum_cube_odd_pipe 5;;
(* - : int = 153 *)
let rec exists x = function 
|[] -> false
|h::t ->if h=x then true else exists x t;;
(* val exists : 'a -> 'a list -> bool = <fun> *)
exists 7 [1;3;43;76];;
(* - : bool = false *)
exists 7 [1;3;43;7];;
(* - : bool = true *)
let exists_fold n lst = List.fold_left (fun acc x -> acc||x=n) false lst;;
(* val exists_fold : 'a -> 'a list -> bool = <fun> *)
exists_fold 7 [1;3;43;7];;
(* - : bool = true *)
exists_fold 7 [1;3;43;72];;
(* - : bool = false *)
let exists_lib x lst = List.exists (fun n -> n=x ) lst;;
(* val exists_lib : 'a -> 'a list -> bool = <fun> *)
exists_lib 7 [1;3;43;72];;
(* - : bool = false *)
exists_lib 7 [1;3;43;7];;
(* - : bool = true *)
(* let balance n lst = n-(List.fold_left (fun x ->x+x) 0 lst);; *)
(* Error: This expression has type int but an expression was expected of type
         'a -> int *)
let balance n lst = n-(List.fold_left (fun acc x ->acc+x) 0 lst);;
(* val balance : int -> int list -> int = <fun> *)
balance 500 [50;32;32;100;36];;
(* - : int = 250 *)
let uncurried_nth (lst, n) = List.nth lst n;;
(* val uncurried_nth : 'a list * int -> 'a = <fun> *)
let uncurried_append (lst1,lst2) = List.append lst1 lst2;;
(* val uncurried_append : 'a list * 'a list -> 'a list = <fun> *)
let uncurried_compare (lst1,lst2) = Char.compare lst1 lst2;;
(* val uncurried_compare : char * char -> int = <fun> *)
let uncurried_max (lst1,lst2) = Stdlib.max lst1 lst2;;
(* val uncurried_max : 'a * 'a -> 'a = <fun> *)
(* uncurried_nth ([1;2;34;54] 3);; *)
(* Error: This expression has type int list
       This is not a function; it cannot be applied. *)
uncurried_nth ([1;2;34;54], 3);;
(* - : int = 54 *)
uncurried_append ([12;32;34],[3254;34;34;43]);;
(* - : int list = [12; 32; 34; 3254; 34; 34; 43] *)
(* uncurried_compare ("d","A");; *)
(* Error: This expression has type string but an expression was expected of type
         char *)
uncurried_compare ('d','A');;
(* - : int = 35 *)
