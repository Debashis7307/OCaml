type day = Sun | Mon | Tue | Wed | Thu | Fri |Sat ;;
(*The individual names of the values of a variant are called constructors*)

let int_of_day = function
| Sun -> 1
| Mon -> 2
| Tue -> 3
| Wed -> 4
| Thu -> 5
| Fri -> 6
| Sat -> 7