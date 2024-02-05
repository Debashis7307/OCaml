(*Use case of Patern Matching*)
let rec sum = function
|[]-> 0
|h::t -> h+ sum t;;
sum [1;2;3;4;5;5;4;3;2;1]