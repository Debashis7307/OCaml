let x = ref 34;;
(* val x : int ref = {contents = 34} *)
x;;
(* - : int ref = {contents = 34} *)
x:=342;;
(* - : unit = () *)
x;;
(* - : int ref = {contents = 342} *)
!x;;
(* - : int = 342 *)
let fact0 = ref(fun x -> x +0);;
(* val fact0 : (int -> int) ref = {contents = <fun>} *)
(* fact0 4;; *)
(* Error: This expression has type (int -> int) ref
       This is not a function; it cannot be applied. *)
(* fact0 !4;; *)
(* Error: This expression has type (int -> int) ref
       This is not a function; it cannot be applied. *)
let fact n = if n = 0 then 1 else n * !fact0 (n-1);;
(* val fact : int -> int = <fun> *)
fact 5;;
(* - : int = 20 *)
(* fact0 = fact;; *)
(* Error: This expression has type int -> int
       but an expression was expected of type (int -> int) ref *)
fact0 := fact;;
(* - : unit = () *)
fact 5;;
(* - : int = 120 *)
List.iter;;
(* - : ('a -> unit) -> 'a list -> unit = <fun> *)
[|34;54;56;56;6787;|];;
(* - : int array = [|34; 54; 56; 56; 6787|] *)
let v = [|34;54;56;56;6787;|];;
(* val v : int array = [|34; 54; 56; 56; 6787|] *)
v.(3);;
(* - : int = 56 *)
v.(3)<- 342;;
(* - : unit = () *)
v;;
(* - : int array = [|34; 54; 56; 342; 6787|] *)
type student = {name: string; mutable gpa: float}

 let alice = {name = "Alice"; gpa = 3.7}

 let () = alice.gpa <- 4.0;;
(* type student = { name : string; mutable gpa : float; }
val alice : student = {name = "Alice"; gpa = 4.} *)
let (_ : bool ref) = ref true
 let (_ : int list ref) = ref [5;3]
 let (_ : int ref list) = [ref 5; ref 3];;
let cs3110 =
   let inc = ref (fun x -> x + 1) in
   !inc 3109;;
(* val cs3110 : int = 3110 *)
let (+:=) x y =
   x := !x + y;;
(* val ( +:= ) : int ref -> int -> unit = <fun> *)
let _ =
   let x = ref 0 in
   let y = x in
   let z = ref 0 in

   assert (x == y);
   assert (not (x == z));
   assert (x = y);
   assert (x = z);
   x := 1;
   assert (x = y);
   assert (not (x = z));;
(* - : unit = () *)
type vector = float array

 (** [norm v] is the Euclidean norm of [v]. *)
 let norm v =
   sqrt (Array.fold_left (fun acc x -> acc +. x ** 2.) 0. v)

 (* another solution:  same asymptotic complexity but
    less efficient.  Perhaps more readable. *)
 let norm' v =
   v
   |> Array.map (fun x -> x ** 2.)  (* square each element *)
   |> Array.fold_left (+.) 0.       (* sum all elements *)
   |> sqrt;;
(* type vector = float array
val norm : float array -> float = <fun>
val norm' : float array -> float = <fun> *)
let normalize v =
   let n = norm v in (* Must calculate norm before iteration *)
   Array.iteri (fun i x -> v.(i) <- x /. n) v
;;
(* val normalize : float array -> unit = <fun> *)
let normalize_loop v =
   let n = norm v in
   for i = 0 to Array.length v - 1 do
     v.(i) <- v.(i) /. n
   done;;
(* val normalize_loop : float array -> unit = <fun> *)
let fact_loop n =
   let ans = ref 1 in
   for i = 1 to n do
     ans := !ans * i
   done;
   !ans;;
(* val fact_loop : int -> int = <fun> *)
let norm_loop v =
   let n = ref 0.0 in
   for i = 0 to Array.length v - 1 do
     n := !n +. (v.(i) ** 2.)
   done;
   sqrt !n;;
(* val norm_loop : float array -> float = <fun> *)
let init_matrix n o f =
   Array.init n (fun i -> Array.init o (fun j -> f i j));;
(* val init_matrix : int -> int -> (int -> int -> 'a) -> 'a array array = <fun> *)
