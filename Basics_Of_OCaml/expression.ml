let h = 50 in h+10 ;;(*one can define or creat any type of function or expression using "let" key-word *)
let x = 42 in(* y is not meaningful here *)
  x + (let y = "3110" in
         (* y is meaningful here *)
         int_of_string y)