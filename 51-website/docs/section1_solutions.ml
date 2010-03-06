(* CS51 Section 1 *)
(* Week of 2/7/10 *)

(* Algebraic Data Types *)
(* Exercise 1 *)
(* Look at the following type definition. *)
type color = Red | Yellow | Blue | Green | Crimson | Other of string
type favorite = Color of color | Movie of string | Tvshow of string |
                Number of float | Letter of char

let a : favorite list = [Movie "A Beautiful Mind"; Color Blue;
                         Tvshow "The Simpsons"; Color Crimson];;

let b : favorite list = [Number 1.0; Number 2.0; Number 5.0;
                         Number 14.0; Number 42.0];;

let c : favorite list = [Movie "Love Story"; Tvshow "On Harvard Time";
                         Letter 'H'; Color Crimson];;

let d : favorite list = [Tvshow "Lost"; Number 3.14];;

let students = [a; b; c; d];;

(* 1a. Define a value of type favorite list for someone whose
 * favorite color is chartreuse and whose favorite number is 5. *)

let prob1a : favorite list = [(* ??? *)];;

(* 1b. Write a function that takes a value of type favorite list (like the
 * ones above) and returns this person's favorite movie, or None if a
 * favorite movie isn't given. If multiple movies are listed, return the first.
 * What return type does this function have?*)

let rec favmovie (lst: favorite list) (*: ???*) =
	match lst with
		| [] -> None
		| (Movie m)::_ -> Some m
		| _::t -> favmovie t
	;;

(* 1c. Write a function that takes a value of type favorite list and returns 
 * true if and only if this person has listed crimson as a favorite color. *)

let rec harvardpride (lst: favorite list) : bool =
	match lst with
		| [] -> false
		| (Color Crimson)::_ -> true
		| _::t -> harvardpride t
	;;

(* 1d. Write a function that takes a list of favorite lists and returns any
   with a favorite color listed as crimson. *) 

let rec harvardfilter (lst: favorite list list) : favorite list list =
	match lst with
		| [] -> []
		| h::t -> if (harvardpride h) then h::(harvardfilter t) 
		          else (harvardfilter t)
	;;

(* Exercise 2 *)
(* 2a. Define an algebraic data type representing either ints or floats *)

type realnum = Int of int | Float of float

(* 2b. Define a function testing whether two realnums are equal. It shouldn't
 * matter whether they are ints or floats, e.g (realequal 4 4.0) => True. *)

let realequal (a: realnum) (b: realnum) : bool =
	match (a, b) with
		| (Int a1, Int b1) -> a1 = b1
		| (Float a1, Float b1) -> a1 = b1
		| (Int a1, Float b1) -> (float_of_int a1) = b1
		| (Float a1, Int b1) -> a1 = (float_of_int b1)
	;;
	
type expr = Value of bool | Not of expr | And of expr * expr | Or of expr * expr

(* 2c. Write a function that takes in an expr and returns whether it evaluates
 * to true or false. *)
let rec eval (a:expr) : bool =
	match a with
		| Value v -> v
		| Not e -> not(eval e)
		| And (e1, e2) -> eval e1 && eval e2
		| Or (e1, e2) -> eval e1 || eval e2
	;;

(* Higher-order functions and polymorphism *)
(* Exercise 3 *)

(* 3a. Write a function to return the smaller of two int options, or None
 * if both are None. *)
let min_option (x: int option) (y: int option) : int option =
	match (x, y) with
		| (None, _) -> y
		| (_, None) -> x
		| (Some x', Some y') -> Some (min x' y')
	;;
	
(* 3b. Write a function to return the larger of two int options, or None
 * if both are None. *)
let max_option (x: int option) (y: int option) : int option =
	match (x, y) with
		| (None, _) -> y
		| (_, None) -> x
		| (Some x', Some y') -> Some (max x' y')
	;;

(* What's the pattern? How can we factor out similar code? *)
(* 3c. Write a higher-order function for binary operations on options. *)
(* What is calc_option's function signature? *)
let calc_option (f: 'a->'a->'a) (x: 'a option) (y: 'a option) : 'a option =
	match (x, y) with
		| (None, _) -> y
		| (_, None) -> x
		| (Some x', Some y') -> Some (f x' y')
	;;

(* 3d. Now rewrite min_option and max_option using the higher-order function. *)
let min_option_2 (x: int option) (y: int option) : int option =
	calc_option min x y;;
	
let max_option_2 (x: int option) (y: int option) : int option =
	calc_option max x y;;

(* Can we use this in other ways? *)

(* 3e. Write a function to return the boolean AND of two bool options,
 * or None if both are None. *)
let and_option (x:bool option) (y: bool option) : bool option =
	calc_option (&&) x y;;

(* 3f. Write a function to return the boolean OR of two bool options,
 * or None if both are None. *)
let or_option (x:bool option) (y: bool option) : bool option =
	calc_option (||) x y;;
	
(* Map and reduce *)
(* Exercise 4 *)
let rec reduce f u xs =
  match xs with
    | [] -> u
    | hd::tl -> f hd (reduce f u tl);;

let rec map f xs =
  match xs with
    | [] -> []
    | hd::tl -> (f hd) :: (map f tl);;
	
(* let map = List.map;; *)

(* 4a. Implement length in terms of reduce. 
 * length lst returns the length of lst. length [] = 0. *)
let length (lst: int list) : int =
	reduce (fun x r -> r + 1) 0 lst
	;;

(* 4b. Write a function that takes an int list and multiplies every int by 3.
 * Use map. *)
let times_3 (lst: int list): int list =
	map (fun a -> a * 3) lst
	;;

(* 4c. Write a function that takes an int list and an int and multiplies every
 * entry in the list by the int. Use map. *)
let times_x (lst: int list) (x : int): int list =
	map (fun a -> a * x) lst
	;;

(* 4d. Rewrite times_3 in terms of times_x.
 * This should take very little code. *)
let times_3_shorter = times_x 3 ;;

(* 4e. Write a function that takes an int list and generates a "multiplication
 * table", a list of int lists showing the product of any two entries in the
 * list. *)
let mult_table (lst: int list) : int list list =
	map (fun a -> times_x a lst) lst
	;;
	
(* 4f. Write a function that takes a list of boolean values
 * [x1; x2; ... ; xn] and returns x1 AND x2 AND ... AND xn.
 * For simplicity, assume and_list [] is TRUE. Use reduce. *)
let and_list (lst: bool list) : bool =
	reduce (&&) true lst
	;;

(* 4g. Do the same as above, with OR.
 * Assume or_list [] is FALSE. *)
let or_list (lst: bool list) : bool =
	reduce (||) false lst
	;;
	
(* 4h.	 Write a function that takes a bool list list and returns
 * its value as a boolean expression in conjunctive normal form (CNF).
 * A CNF expression is represented as a series of OR expressions joined
 * together by AND.
 * e.g. (x1 OR x2) AND (x3 and x4 and x5) AND (x6).
 * Use map and/or reduce.
 * You may find it helpful to use and_list and or_list. *)
let cnf_list (lst: bool list list) : bool =
	and_list (map or_list lst)
	;;
	
(* 4i. Write a function that takes an expr list and returns true if and only if
 * every expr in the list represents a true Boolean expression. *)
let all_true (lst: expr list) : bool =
	reduce (&&) true (map eval lst)
	;;
	
(* 4j. Write and_list to return a bool option,
 * where the empty list yields None. Use reduce. *)
let and_list_smarter (lst: bool list) : bool option =
	reduce (fun x base -> and_option (Some x) base) None lst
	;;

let and_list_smartest (lst: bool list) : bool option =
	reduce and_option None (map (fun x -> Some x) lst)
	;;

(* 4k. Write max_of_list from section 0:
 * Return the max of a list, or None if the list is empty. *)
let max_of_list (lst:int list) : int option =
	reduce max_option None (map (fun x -> Some x) lst)
	;;

(* 4l. Write bounds from section 0:
 * Return the min and max of a list, or None if the list is empty. *)
let bounds (lst:int list) : (int option * int option) =
	reduce (fun x (small, big) -> (min_option x small, max_option x big))
	       (None, None)
		   (map (fun x -> Some x) lst)
	;;
