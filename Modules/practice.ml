module Mods = struct
  let b = "bigred"
  let inc x = x + 1
  module M = struct
    let y = 42
  end
end;;
(* module Mods :
  sig val b : string val inc : int -> int module M : sig val y : int end end *)
Mods. inc 34;;
(* - : int = 35 *)
(* Mods.M.y 98342;; *)
(* Error: This expression has type int
       This is not a function; it cannot be applied. *)
Mods.M.y;;
(* - : int = 42 *)
module Math  = struct 
let rec fact n acc = if n = 0 then acc else fact (n-1) (n*acc)
let fact_f n = fact n 1
end;;
(* module Math : sig val fact : int -> int -> int val fact_f : int -> int end *)
open Math;;
fact_f 5;;
(* - : int = 120 *)
module type MATH = sig
  (** [fact n] is [n!]. *)
  val fact : int -> int
end

module Math = struct
  (** [fact_aux n acc] is [n! * acc]. *)
  let rec fact_aux n acc =
    if n = 0 then acc else fact_aux (n - 1) (n * acc)

  let fact n = fact_aux n 1
end

module MathCheck : MATH = Math;;
(* module type MATH = sig val fact : int -> int end
module Math : sig val fact_aux : int -> int -> int val fact : int -> int end
module MathCheck : MATH *)
(* module BankAccount =
  sig
    type account = { balance: float }

    val create : float -> account
    val deposit : account -> float -> account
    val withdraw : account -> float -> account option
  end

module BankAccountImpl =
  struct
    type account = { balance: float }

    let create balance = { balance }

    let deposit account amount =
      if amount >= 0.0 then { account with balance = account.balance +. amount }
      else failwith "Invalid deposit amount: cannot be negative"

    let withdraw account amount =
      if amount >= 0.0 && amount <= account.balance then
        Some { account with balance = account.balance -. amount }
      else None
  end

include BankAccountImpl with type account = account
;; *)
(* Error: Syntax error *)
(* module BankAccount =
  sig
    type account = { balance: float }

    val create : float -> account
    val deposit : account -> float -> account
    val withdraw : account -> float -> account option
  end

module BankAccountImpl =
  struct
    type account = { balance: float }

    let create balance = { balance }

    let deposit account amount =
      if amount >= 0.0 then { account with balance = account.balance +. amount }
      else failwith "Invalid deposit amount: cannot be negative"

    let withdraw account amount =
      if amount >= 0.0 && amount <= account.balance then
        Some { account with balance = account.balance -. amount }
      else None
  end;; *)
(* Error: Syntax error *)
module type BankAccount =
  sig
    type account = { balance: float }

    val create : float -> account
    val deposit : account -> float -> account
    val withdraw : account -> float -> account option
  end

module BankAccountImpl =
  struct
    type account = { balance: float }

    let create balance = { balance }

    let deposit account amount =
      if amount >= 0.0 then { account with balance = account.balance +. amount }
      else failwith "Invalid deposit amount: cannot be negative"

    let withdraw account amount =
      if amount >= 0.0 && amount <= account.balance then
        Some { account with balance = account.balance -. amount }
      else None
  end;;
(* module type BankAccount =
  sig
    type account = { balance : float; }
    val create : float -> account
    val deposit : account -> float -> account
    val withdraw : account -> float -> account option
  end
module BankAccountImpl :
  sig
    type account = { balance : float; }
    val create : float -> account
    val deposit : account -> float -> account
    val withdraw : account -> float -> account option
  end *)
module type T = sig
  type t = int
  val x : t
end

module M : T = struct
  type t = int
  let x = 42
end

let a : int = M.x;;
(* module type T = sig type t = int val x : t end
module M : T
val a : int = 42 *)
List.init 5 (fun n -> List.init n (Fun.const n));;
(* - : int list list = [[]; [1]; [2; 2]; [3; 3; 3]; [4; 4; 4; 4]] *)
(* List.init 5 (fun n -> List.init n+1 ( Fun.const n+1));; *)
(* Error: This expression has type (int -> 'a) -> 'a list
       but an expression was expected of type int *)
(* List.init 5 (fun n -> List.init (n+1) ( Fun.const n+1));; *)
(* Error: This expression has type 'a -> int
       but an expression was expected of type int *)
List.init 5 (fun n -> List.init (n+1) ( Fun.const (n+1)));;
(* - : int list list = [[1]; [2; 2]; [3; 3; 3]; [4; 4; 4; 4]; [5; 5; 5; 5; 5]] *)
let s = "my priority is difference from others."
let l = String.length s;;
(* val s : string = "my priority is difference from others."
val l : int = 38 *)
let lst = [1; 2];;
(* val lst : int list = [1; 2] *)
let lst' = List.tl lst;;
(* val lst' : int list = [2] *)
module type Stack = sig
  type 'a t
  exception Empty
  val empty : 'a t
  val is_empty : 'a t -> bool
  val push : 'a -> 'a t -> 'a t
  val peek : 'a t -> 'a
  val pop : 'a t -> 'a t
  val size : 'a t -> int
  val to_list : 'a t -> 'a list
end

module ListStack : Stack = struct
  type 'a t = 'a list
  exception Empty
  let empty = []
  let is_empty = function [] -> true | _ -> false
  let push = List.cons
  let peek = function [] -> raise Empty | x :: _ -> x
  let pop = function [] -> raise Empty | _ :: s -> s
  let size = List.length
  let to_list = Fun.id
end;;
(* module type Stack =
  sig
    type 'a t
    exception Empty
    val empty : 'a t
    val is_empty : 'a t -> bool
    val push : 'a -> 'a t -> 'a t
    val peek : 'a t -> 'a
    val pop : 'a t -> 'a t
    val size : 'a t -> int
    val to_list : 'a t -> 'a list
  end
module ListStack : Stack *)
open ListStack;;
let s = empty |> push 1 |> push 2;;
(* val s : int t = <abstr> *)
let s' = pop s;;
(* val s' : int t = <abstr> *)
to_list s;;
(* - : int list = [2; 1] *)
to_list s';;
(* - : int list = [1] *)
Fun.id;;
(* - : 'a -> 'a = <fun> *)
Fun.id [1;2;4;5];;
(* - : int list = [1; 2; 4; 5] *)
module type Set = sig
  (** ['a t] is the type of sets whose elements are of type ['a]. *)
  type 'a t

  (** [empty] is the empty set *)
  val empty : 'a t

  (** [mem x s] is whether [x] is an element of [s]. *)
  val mem : 'a -> 'a t -> bool

  (** [add x s] is the set that contains [x] and all the elements of [s]. *)
  val add : 'a -> 'a t -> 'a t

  (** [elements s] is a list containing the elements of [s].  No guarantee
      is made about the ordering of that list, but each is guaranteed to
      be unique. *)
  val elements : 'a t -> 'a list
end;;
(* module type Set =
  sig
    type 'a t
    val empty : 'a t
    val mem : 'a -> 'a t -> bool
    val add : 'a -> 'a t -> 'a t
    val elements : 'a t -> 'a list
  end *)
module ListSet : Set = struct
  type 'a t = 'a list
  let empty = []
  let mem = List.mem
  let add = List.cons
  let elements s = List.sort_uniq Stdlib.compare s
end;;
(* module ListSet : Set *)
module type XY = sig
  type x
  type y
end

module type T = sig
  module A : XY
end

module B = struct
  type x = int
  type y = float
end

module type U = T with module A = B

module C : U = struct
  module A = struct
    type x = int
    type y = float
    let x = 42
  end
end;;
(* module type XY = sig type x type y end
module type T = sig module A : XY end
module B : sig type x = int type y = float end
module type U = sig module A : sig type x = int type y = float end end
module C : U *)
(* module type L = sin
val x : int
end
module M = struct
let inc n -> n+1
end;; *)
(* Error: Syntax error *)
(* module type L = sin
val x : int -> int
end
module M = struct
let inc n -> n+1
end;; *)
(* Error: Syntax error *)
(* module type L = sin
val l : int -> int
end
module M = struct
let inc l -> l+1
end;; *)
(* Error: Syntax error *)
(* module type L = sin
val x : int
end
module M = struct
let inc n =1
end;; *)
(* Error: Syntax error *)
(* module type L = sin
val x : int
end
module M:n = struct
let inc n =1
end;; *)
(* Error: Syntax error *)
(* module type L = sin
val x : int
end
module M:n = struct
let  n =1
end;; *)
(* Error: Syntax error *)
(* module type L = sin
val x : int
end
module M:n = struct
let n =0
end;; *)
(* Error: Syntax error *)
module type X = sig
  val x : int
end

module IncX (M : X) = struct
  let x = M.x + 1
end;;
(* module type X = sig val x : int end
module IncX : functor (M : X) -> sig val x : int end *)
