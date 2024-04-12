(* 
let rec fib n =
  if n=0 then 0
  else if n=1 then 1
  else fib (n-1) + fib (n-2);; *)

  let rec fib n =
    match n with
    |0->1
    |1->1
    |n->fib(n-1)+fib(n-2)