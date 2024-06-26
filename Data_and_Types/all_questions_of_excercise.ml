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

  (* * exercise: take drop *)
  let rec take n lst =
  if n = 0 then [] else match lst with
  |[] -> []
  |h::t -> h:: take (n-1) t

  let rec drop n lst =
    if n = 0 then lst else match lst with 
    |[]->[]
    |h::t -> drop (n-1) t

  (* * exercise: take drop tail*)
let rec take_rec n lst acc =
  if n =0 then acc else match lst with 
  |[]->[]
  |h::t -> take_rec (n-1) t (h::acc)

let take_final n lst =
  List.rev(take_rec n lst [])

  (* * exercise: powerset *)
  let rec powerset = function
  |[]->[[]]
  |h::t -> let p = powerset t in List.map (List.cons h) p @ p

  (* * exercise: print int list rec *)
  let rec print_int_list = function
| [] -> ()
| h :: t -> print_endline(string_of_int h); print_int_list t

(* * exercise: print int list iter *)
let print_int_list' lst =
  List.iter (fun x -> print_endline (string_of_int x)) lst

  (* * exercise: student *)
  type student = {first_name : string ; last_name : string ; gpa :float}
  let s  = { first_name = "Debashis"; last_name = "Bera"; gpa = 9.2}
  let s_name s = s.first_name, s.last_name
  let sa_sudent f l g = {first_name=f;last_name=l;gpa=g}

  (* * exercise: pokerecord *)
  type poketype = Normal | Fire | Water
  type pokemon ={name:string ; hp:int ;p:poketype}
  let charizard = {name="charizard";hp=78;p=Fire}
  let squirtle = {name="squirtle";hp=44;p=Water}

  (* * exercise: safe hd and tl *)
  let safe_hd = function 
  |[]-> None 
  |h::t -> Some h

  let safe_tl = function 
  |[]-> None 
  |h::t -> Some t

  (* * exercise: pokefun *)
let rec max_hp = function 
|[]->None 
|h::t -> begin
  match max_hp t with
  |None-> Some h
  |Some h1 -> Some ( if h.hp>=h1.hp then h else h1)
end

(* * exercise: date before *)
type date = int*int*int 
let is_before date1 date2 = 
  let (y1,m1,d1) = date1 in
  let (y2,m2,d2) = date2 in 
  y1>y2 || (y1=y2 && m1>m2) || (y1=y2 && m1=m2 && d1>d2)

  (* * exercise: earliest date *)
let rec  earliest = function 
|[] ->None 
|h::t -> begin 
  match earliest t with
  |None -> Some h
  |Some h1 -> Some(if is_before h h1 then h else h1)
end

(* * exercise: cards *)
type suit = Hearts | Spades | Clubs | Diamonds

type rank = Number of int | Ace | Jack | Queen | King

type card = { suit: suit; rank: rank }

let ace_of_clubs : card = { suit = Clubs; rank = Ace }
let queen_of_hearts : card = { suit = Hearts; rank = Queen }
let two_of_diamonds : card = { suit = Diamonds; rank = Number 2 }
let seven_of_spades : card = { suit = Spades; rank = Number 7 }

(* * exercise: quadrant *)
type sqad = I |II |III |IV 
type sing = Pos | Zero | Neg 
let sin x = if x=0 then Zero else if x<0 then Neg else Pos
let quaderent (x,y) =
  match sin x , sin y with
  |Pos,Pos->Some I
  |Neg,Pos->Some II
  |Neg,Neg->Some III
  |Pos,Neg->Some IV
  |_->None


(* * exercise: quadrant when  *)
let quadrant_when = function
  | x,y when x > 0 && y > 0 -> Some I
  | x,y when x < 0 && y > 0 -> Some II
  | x,y when x < 0 && y < 0 -> Some III
  | x,y when x > 0 && y < 0 -> Some IV
  | _ -> None

(* * exercise: depth *)
type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree 
let rec defth = function 
|Leaf -> 0
|Node(_,left,right) -> 1+ max (defth left) (defth right)
let t =
  Node(4,
    Node(2,
      Node(1, Leaf, Leaf),
      Node(3, Leaf, Leaf)
    ),
    Node(5,
      Node(6, Leaf, Leaf),
      Node(7, Leaf, Leaf)
    )
  );;

  (* * exercise: shape *)
  let rec same_shape t1 t2 =
    match t1, t2 with
    | Leaf, Leaf -> true
    | Node (_,l1,r1), Node (_,l2,r2) -> (same_shape l1 l2) && (same_shape r1 r2)
    | _ -> false

(* * exercise: list max exn *)
let rec max_list = function 
|[]-> raise(Failure "can't be possible") 
|[n]->n
|h1::h2::t -> if h1>h2 then max_list (h1::t) else max_list(h2::t)

  (* * exercise: quadrant poly *)
  let sign_poly x : [> `Neg | `Pos | `Zero] =
    if x < 0 then `Neg
    else if x = 0 then `Zero
    else `Pos
  
  let quadrant (x,y) : [> `I | `II | `III | `IV ] option =
    match sign_poly x, sign_poly y with
    | `Pos, `Pos -> Some `I
    | `Neg, `Pos -> Some `II
    | `Neg, `Neg -> Some `III
    | `Pos, `Neg -> Some `IV
    | _ -> None
