(* List.map f (List.map g lst);; *)
(* Error: Unbound value f *)
(* List.map f (List.map g x) lst;; *)
(* Error: This function has type ('a -> 'b) -> 'a list -> 'b list
       It is applied to too many arguments; maybe you forgot a `;'. *)
(* let c f g= List.map f (List.map g lst);; *)
(* Error: Unbound value lst
Hint: Did you mean fst, lsl or lsr? *)
let c f g lst = List.map f (List.map g lst);;
(* val c : ('a -> 'b) -> ('c -> 'a) -> 'c list -> 'b list = <fun> *)
let c f g lst = List.map (g |> f) lst;;
(* val c : ('a -> 'b -> 'c) -> 'a -> 'b list -> 'c list = <fun> *)
let c f g lst = List.map (fun x -> f (g x )) lst;;
(* val c : ('a -> 'b) -> ('c -> 'a) -> 'c list -> 'b list = <fun> *)
let c f g lst = List.map (fun x -> x|> g |> f) lst;;
(* val c : ('a -> 'b) -> ('c -> 'a) -> 'c list -> 'b list = <fun> *)
let c = "debashis ekta paglachoda...";;
(* val c : string = "debashis ekta paglachoda..." *)
String.copy c;;
(* - : string = "debashis ekta paglachoda..." *)
let l = [4;2;5;56;67;87;89];;
(* val l : int list = [4; 2; 5; 56; 67; 87; 89] *)
let fined lst = List.filter (fun x -> x>5) lst;;
(* val fined : int list -> int list = <fun> *)
fined l;;
(* - : int list = [56; 67; 87; 89] *)
let s = ["hi";"gy"];;
(* val s : string list = ["hi"; "gy"] *)
let sap lst sep = match lst with
|[] -> "";
|h :: t -> List.fold_left (fun acc x -> acc ^sep ^ x) h t;;
(* val sap : string list -> string -> string = <fun> *)
sap s ",";;
(* - : string = "hi,gy" *)
List.filter;;
(* - : ('a -> bool) -> 'a list -> 'a list = <fun> *)
let li = [1.1;2.1;3.1];;
(* val li : float list = [1.1; 2.1; 3.1] *)
let add_f lst = List.map (fun x -> x +. 3.9) lst;;
(* val add_f : float list -> float list = <fun> *)
add_f li;;
(* - : float list = [5.; 6.; 7.] *)
module type matrix = sig
type t 
val add : t -> t -> t
end;;
(* module type matrix = sig type t val add : t -> t -> t end *)
module ADDX(M : matrix) = struct
let add_lists l1 l2 = List.map2 (fun x y -> M.add x y) l1 l2
let add_matrices m1 m2 = List.map2 (fun x y -> add_lists x y) m1 m2;;
end;;
(* module ADDX :
  functor (M : matrix) ->
    sig
      val add_lists : M.t list -> M.t list -> M.t list
      val add_matrices : M.t list list -> M.t list list -> M.t list list
    end *)
module INTMATRIX : matrix = struct 
type t = int
let add = (+)
end;;
(* module INTMATRIX : matrix *)
module FLOATMATRIX : matrix = struct
type t = float
let add = (+.)
end;;
(* module FLOATMATRIX : matrix *)
module IntMatrixAdd = ADDX(INTMATRIX);;
(* module IntMatrixAdd :
  sig
    val add_lists : INTMATRIX.t list -> INTMATRIX.t list -> INTMATRIX.t list
    val add_matrices :
      INTMATRIX.t list list -> INTMATRIX.t list list -> INTMATRIX.t list list
  end *)
IntMatrixAdd.add_matrices;;
(* - : INTMATRIX.t list list -> INTMATRIX.t list list -> INTMATRIX.t list list =
<fun> *)
