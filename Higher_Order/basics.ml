(** [add1 lst] adds 1 to each element of [lst] *)
let rec add1 = function
  | [] -> []
  | h :: t -> (h + 1) :: add1 t

let lst1 = add1 [1; 2; 3]

(*map function in tail recursive*)
let rec mapp f acc = function 
|[]-> acc 
|h::t-> mapp f (acc@[f h]) t

let map_tr f = mapp f [] 

(** [even n] is whether [n] is even. *)
let even n =
  n mod 2 = 0

(** [evens lst] is the sublist of [lst] containing only even numbers. *)
let rec evens = function
  | [] -> []
  | h :: t -> if even h then h :: evens t else evens t

let lst1 = evens [1; 2; 3; 4]

let rec filter p = function
  | [] -> []
  | h :: t -> if p h then h :: filter p t else filter p t