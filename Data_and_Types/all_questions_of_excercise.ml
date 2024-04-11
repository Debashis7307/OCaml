(* * exercise: list expressions *)
let l1=[1;2;3;4;5;]
let l2=1::2::3::4::5::[]
let l3=[1]@[2;3;4;5]

(* * exercise: product *)
let rec product = function 
|[]->1
|h::t->h*product t

(* * exercise: concat *)
let rec concat =function 
|[]->" "
|h::t -> h ^ concat t

(* *   unit test *)
assert(product l1 = 120)

(* * exercise: patterns *)
let rec find = function 
|[]->false 
|h::t->if h="bigred" then true else false 


let rec two_four = function 
|_::_::[] -> true
|_::_::_::_::[] -> true
|_->false 

let rec check = function 
|[]->false 
|h1::h2::_->if h1=h2 then true else false 
|_->false 

(* * exercise: library *)
let fifth lst =if(List.length lst) >= 5 then List.nth lst 4 else 0 

let sorting lst = List.rev (List.sort Stdlib.compare lst)

(* * exercise: library puzzle *)
let last_ele lst = List.hd(List.rev lst)

let if_zeros lst = List.exists(fun x->x=0) lst