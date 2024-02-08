
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