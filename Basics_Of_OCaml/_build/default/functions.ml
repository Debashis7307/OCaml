let f = fun x y -> (x +. y)/. 2.;;
(*two ways to define a function of average*)
let g x y = (x+.y)/. 2.;;
f 43. 43.;;
g 32. 65.;;

(*Code of factorial by using recursion "rec" key words*)

let rec factorial n =
  if n = 0 then 1 
  else n*factorial(n-1);;

(*code for power*)

let rec power a b =
  if b=0 then 1
  else a*power a (b-1);;

let rec even n =
  n = 0 || odd (n-1)
  and odd n =
    n <> 0 && even (n-1);;

(* Anonymous Functions*)
(fun x y -> x mod y) 432 10;;

(*Pipeline*)
let square x = x*x;;
5 |> square |> succ |> square |> succ;;