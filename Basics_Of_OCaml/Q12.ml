let f x = if x then x else x
(* bool -> bool *)
(* x must be a bool to to be used as the conditional in the if expression *)

let g x y = if y then x else x
(* 'a -> bool -> 'a *)
(* x could have any type *)

let h x y z = if x then y else z
(* bool -> 'a -> 'a -> 'a *)
(* both branches of the if expression must have the same type,
 *  so y and z must have the same type (which could be anything) *)

let i x y z = if x then y else y
(* bool -> 'a -> 'b -> 'a *)
(* z could have any type, and moreover, that type could be different
 *  than the type of y *)