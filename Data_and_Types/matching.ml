let x =
  match not true with
  | true -> "nope"
  | false -> "yep";;

let y =
  match 42 with
  | fooo -> fooo;;

let a = 
  match "foo" with
  | "bar" -> 0
  | _ -> 1  ;;  (*_ is called wild card*)

let b =
  match [1;2] with
  | [] -> "empty"
  |_ -> "not empty";;

let c = 
  match ["dev";"alak";"moni"] with
  | []->["empty"]
  | h :: t -> t;;

let fst3 t =
  match t with 
  |(a,b,c) -> a;;

type student = {
  name : string;
  year : int;
}

let makaut = {
  name="Debashis";
  year=2026;
}

let motch s =
  match s with 
  | {name;year} -> name ^ " '" ^ string_of_int year;;

let empty lst =
  match lst with 
  |[]-> true
  |h::t -> false;;