let define n1 n2 = if n1 = n2 then 0 else if n1<n2 then -1 else 1;;
(* val define : 'a -> 'a -> int = <fun> *)
type nat = Zero | Succ of nat;;
(* type nat = Zero | Succ of nat *)
Zero;;
(* - : nat = Zero *)
Succ Zero;;
(* - : nat = Succ Zero *)
Succ (Succ Zero);;
(* - : nat = Succ (Succ Zero) *)
let rec int_of_nat (n:nat) = match n with
|Zero -> 0;
|Succ p -> 1 + int_of_nat p;;
(* val int_of_nat : nat -> int = <fun> *)
(* let rec int_of_nat (n:nat) = match n with
|Zero -> 0;
|Succ _ -> 1 + int_of_nat p;; *)
(* Error: Unbound value p *)
let rec int_of_nat (n:nat) = match n with
|Zero -> 0;
|Succ p -> 1 + int_of_nat p;;
(* val int_of_nat : nat -> int = <fun> *)
(* int_of_nat Zero < int_of_nat Succ(Zero);; *)
(* Error: This function has type nat -> int
       It is applied to too many arguments;
       maybe you forgot a `;'. *)
(* int_of_nat Zero < int_of_nat Succ Zero;; *)
(* Error: This function has type nat -> int
       It is applied to too many arguments;
       maybe you forgot a `;'. *)
int_of_nat Zero < int_of_nat (Succ Zero);;
(* - : bool = true *)
let find n1 n2 = if int_of_nat n1 = int_of_nat n2 then 0 else if int_of_nat n1>int_of_nat n2 then 1 else -1;;
(* val find : nat -> nat -> int = <fun> *)
let rec n_th lst n = 
match lst,n with
|[],_-> failwith "The list is empty"
|h::t,0 ->h
|h::t,_->n_th t (n-1);;
(* val n_th : 'a list -> int -> 'a = <fun> *)
n_th [2;3;4;54;65;56] 4;;
(* - : int = 65 *)
(* let rec n_th lst n = 
match lst,n with
|[],_-> None
|h::t,0 ->Some h
|h::t,_->Some (n_th t (n-1));; *)
(* Error: This expression has type 'a option
       but an expression was expected of type
         'a
       The type variable 'a occurs inside
       'a option *)
(* let rec n_th lst n :option= 
match lst,n with
|[],_-> None
|h::t,0 ->Some h
|h::t,_->Some (n_th t (n-1));; *)
(* Error: The type constructor option
       expects 1 argument(s),
       but is here applied to 0 argument(s) *)
let rec n_th lst n = 
match lst,n with
|[],_-> None
|h::t,0 ->Some h
|h::t,_->n_th t (n-1);;
(* val n_th : 'a list -> int -> 'a option =
  <fun> *)
