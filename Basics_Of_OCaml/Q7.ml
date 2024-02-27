
let cube x:float = x*.x*.x;;


let _ = assert (cube 0. = 0.)
let _ = assert (cube 1. = 1.)
let _ = assert (cube 3. = 27.)

let sing x = 
  if x > 0 then 1
  else if x < 0 then -1
  else 0;;

let _ = assert (sing 0 = 0)
let _ = assert (sing (-5) = -1)
let _ = assert (sing 32= 1)


let pi = 3.1415;;
let circle r =
  pi *. r *. r;; 

let close_enough a b =
  Float.abs (a -. b) < 1e-5
let _ = assert (close_enough (circle 1.0) pi)
let _ = assert (close_enough (circle (Float.sqrt (1. /. pi))) 1.)

let ave x y z = (x +. y +. z)/. 3.;;