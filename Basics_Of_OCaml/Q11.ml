let rec h n pp p =
  if n =1 then p
  else h (n-1) p (pp+p);;

let first_fib n =
  if n = 0 then 0 
  else h n 0 1;;