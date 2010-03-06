(* Map a function f across a list.  So:
 *    map f [x1; x2; ...; xn] = [(f x1); (f x2); ...; xn] 
 *)
let rec map f xs = 
  match xs with 
    | [] -> []
    | h::t -> (f h)::(map f t)
;;

(* Reduce a list by combining the elements with the function f -- 
 * use the value u when the list is empty. 
 *   reduce f u [x1; x2; ... ; xn] = f x1 (f x2 (...(f xn u)))
 *)
let rec reduce f u xs = 
  match xs with 
    | [] -> u
    | h::t -> f h (reduce f u t)
;;

(* Write a function that takes a list of pairs of ints and produces
 * the list of pair-wise sums.  For instance:
 *   list_add [(1,3); (4,2); (3,0)] = [4; 6; 3]
 *)
let rec list_add (xs: (int*int) list) : int list = 
  match xs with 
    | [] -> []
    | (x,y)::t -> (x+y)::(list_add t)
;;

let list_add = map (fun (x,y) -> x+y) ;;

(* Write a function that takes a list of pairs of ints and produces
 * the quotient pair-wise, if it exists.  For instance:
 *   list_div [(1,3); (4,2); (3,0)] = [Some 0; Some 2; None]
 *)
let rec list_div (xs:(int*int) list) : int option list = 
  match xs with
    | [] -> []
    | (x,y)::t -> (if y = 0 then None else (Some(x/y)))::(list_div t)
;;

let list_div = map (fun (x,y) -> if y = 0 then None else Some(x/y))
(* Write a function that takes a list of optional integers and
 * which filters out all of the None's, and strips out the Some's.
 * For instance:
 *    filter_none [Some 0; Some 2; None] = [0; 2]
 *)
let rec filter_none (xs: int option list) : int list = 
  match xs with 
    | [] -> []
    | h::t -> (match h with 
                 | None -> filter_none t
                 | Some i -> i::(filter_none t))
;;

let filter_none = reduce (fun (h:int option) (rt:int list) -> 
                            match h with
                              | None -> rt
                              | Some i -> i::rt) [];;

(* Using only reduce, write a function to produce the sum of the
 * squares of a list of numbers. 
 *)


(* Natural numbers as a datatype *)
type nat = Zero | Succ of nat ;;

let zero = Zero ;;
let one = Succ(Zero) ;;
let two = Succ(Succ(Zero)) ;;
let three = Succ(two) ;;

(* write a function to add two nats *)
let rec add (n:nat) (m:nat) : nat = 
  match n with 
    | Zero -> m
    | Succ p -> add p (Succ m)
;;

let rec mul (n:nat) (m:nat) : nat = 
  match n with 
    | Zero -> Zero
    | Succ p -> add m (mul p m)
;;

let rec reduce_nat f u n = 
  match n with 
    | Zero -> u
    | Succ p -> f (reduce_nat f u p)
;;

(* rewrite add in terms of reduce_nat *)
let add n m = reduce_nat (fun p -> Succ p) m n
(* rewrite mul in terms of reduce_nat *)

let rec even (n:nat) : bool = 
  match n with 
    | Zero -> true
    | Succ p -> not (even p)
;;

let rec odd (n:nat) : bool = 
  match n with
    | Zero -> false
    | Succ p -> not (even p)
;;

(* rewrite even and odd in terms of reduce_nat *)
let even = reduce_nat not true
let odd = reduce_nat not false
