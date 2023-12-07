(* Stuff from homework *)

type bstree =
  | Lf
  | Br of int * bstree * bstree ;;

let rec insert (t : bstree) (i : int) : bstree =
  match t with
  | Lf => Br (i, Lf, Lf)
  | Br (n, left, right) =>
    if i = n then t
    else if i < n then Br (n, insert left i, right)
    else Br (n, left, insert right i) ;;

(* Factorial implementation *)

let rec fact n = 
  if n = 0 
  then 1 
  else n * fact (n - 1);;

let _ = print_string (string_of_int (fact 5));;

(* Weekdays *)

type day =
  | Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday ;;

let weekday (d : day) : bool =
  match d with
  | Sunday => false
  | Saturday => false
  | _ => true ;;

let _ = print_string (string_of_bool weekday Sunday) ;;