(*** CS 51 Problem Set 1 ***)
(*** February 9, 2010 ***)
(*** Solution Set ***)


(*>* Problem 1a *>*)

let prob1a : (string * int) list = [("CS", 50); ("CS", 51)] ;;

(*** You need those parentheses! "string * int list" gets parsed as a
     pair of a string and an int list. ***)


(*>* Problem 1b *>*)

let prob1b : float = (fun (x, y) -> x *. y) (4.0, 5.0) ;;

(*** The first part of this expression has type float * float ->
     float, but it's then being applied to a value of type float *
     float. This means that the ultimate result is just a float. ***)

(*>* Problem 1c *>*)

let prob1c : int option * 'a option = (Some 123, None) ;;

(*** We accepted int option * int option here, because it results in
     your solution compiling. However, technically there's no way of
     knowing what type of option "None" is, so we should really
     annotate it with 'a to denote that it's a polymorphic option
     (i.e., it could have any type). ***)


(*>* Problem 1d *>*)

let prob1d : ((int * float) list * string) list = [([(17, 42.0)], "foobar")] ;;

(*** Watch your parentheses and brackets! ***)


(*>* Problem 1e *>*)

let prob1e : (float -> float -> float) * (int * int -> int) =
  ((+.), fun (x, y) -> x + y)
;;

(*** Again, be careful with parentheses. One common mistake was to
     have the second function take its arguments separately rather
     than as a tuple; another was to write:
     
     (fun x y -> x +. y, fun (a, b) -> a + b)

     Though this seems correct, it will have type float -> float ->
     (float * (int * int) -> int). Why? Because without parentheses,
     OCaml parses it as:

     fun x y -> (x +. y, fun (a, b) -> a + b)

     Thus, this solution should parenthesized, e.g., as follows:
     
     ((fun x y -> x +. y), (fun (a, b) -> a + b))
***)    


(*>* Problem 1f *>*)

let prob1f =
  let rec foo bar =
    match bar with
    | ((a, b), c) :: xs -> a * (b + c + (foo xs))
    | _ -> 1
  in foo ([((5, 2), 3)] : ((int * int) * int) list)
;;

(*** The problem statement here was unclear, but we were looking for
     you to pass something of the correct type to foo, making prob1f
     an int. Other compiling solutions were accepted. ***)


(*>* Problem 1g *>*)

let prob1g =
  let v = (32.0, 28.0) in
  let square x = x *. x in
  let squared_norm (w:float * float) : float =
    match w with (f1,f2) -> square f1 +. square f2 in
  let d = sqrt (squared_norm v) in
  int_of_float d
;;

(*** There was no real pattern in mistakes here. ***)


(*>* Problem 2 *>*)

let rec merge (a:int list) (b:int list) : int list =
  match a, b with
  | (a1 :: a'), (b1 :: b') ->
      if a1 < b1 then (a1 :: merge a' b) else (b1 :: merge a b')
  | a, [] -> a
  | [], b -> b
;;

(*** Most common style mistake here was a redundant case: ***)

let rec merge' (a:int list) (b: int list) : int list =
  match a, b with
    | [], [] -> [] (* Unnecessary case - see the next two *)
    | [], _ -> b
    | _, [] -> a
    | (a1 :: a'), (b1 :: b') ->
	if a1 < b1 then (a1 :: merge a' b) else (b1 :: merge a b')
;;

(*** Several people also got the comparison operator wrong, using >
     instead of <. ***)

(*** Many people also thought that merge was the same as append, or
     cons, which it isn't (it combines the lists and sorts
     them). Thus, any solution of the following form, including ones
     that use @ instead of ::, is incorrect: ***)

let rec merge'' (a:int list) (b:int list) : int list =
  match a, b with
    | _, [] -> a
    | [], _ -> b
    | hd::tl, _ -> hd::merge tl b
;;

(*>* Problem 3a *>*)

let rec sorted (l:int list) : bool =
  match l with
  | a :: ((b :: _) as l') -> a <= b && sorted l'
  | _ -> true
;;

(*** By far the most common mistake here was forgetting to use <=
     instead of <. It's also good style to use the short-circuiting
     behavior of the && operator as shown above, avoiding the need for
     an if/then/else construct. ***)

(*>* Problem 3b *>*)

let rec partition (l:int list) (pivot:int) : int list * int list =
  match l with
  | x :: xs ->
      let (less, greater) = partition xs pivot in
      if x <= pivot then (x :: less, greater) else (less, x :: greater)
  | [] -> ([], [])
;;

(*** This was good on the whole. Again, as above, many people used <
     instead of <=, resulting in occasional incorrect behavior. ***)


(*>* Problem 3c *>*)

let rec unzip (l:(int * int) list) : int list * int list =
  match l with
  | (a, b) :: ps -> let (l1, l2) = unzip ps in (a :: l1, b :: l2)
  | [] -> ([], [])
;;

(*** No real consistent pattern of mistakes here. ***)


(*>* Problem 3d *>*)

let rec sum (l:float list) : float =
  match l with
  | x :: xs -> x +. sum xs
  | [] -> 0.0

let variance (l:float list) : float option =
  let n = List.length l in
  if n < 2 then None else Some
    (let nf = float n in
     let mean = sum l /. nf in
     let square x = x *. x in
     (1.0 /. (nf -. 1.0)) *. sum (List.map (fun x -> square (x -. mean)) l))
;;

(*** There were many solutions here, using any number of helper
     functions from 0 to 4+. In general, correct solutions received
     full credit. It's good style to let-bind very small functions
     (e.g., square) rather than defining them separately. Also avoid
     redefining library functions (e.g., List.length), and use
     higher-order functions (e.g., List.map) where appropriate. ***)


(*>* Problem 3e *>*)

let perfect (n:int) : bool =
  let rec sum_divs_from (start:int) : int =
    if start >= n then 0 else
      (if n mod start == 0 then start else 0) + sum_divs_from (start + 1)
  in n == sum_divs_from 1
;;

(*** Again, there was a wide variety of correct solutions
     here. Solutions that attempted to stop counting factors upon
     reaching the square root of the input were more efficient, but
     tended to get quite complicated. Either approach is acceptable,
     but be careful - if you find yourself writing a function longer
     than ~10 lines, you should rethink your approach and strongly
     consider using helper functions.

     Additionally, solutions that found a list of all perfect numbers
     under 4,294,967,295 and simply checked whether the input was
     equal to one of those numbers, while clever, were not
     sufficient. Your code should be portable to any operating system
     and backwards-compatible future OCaml implementation; in
     particular, a 64-bit system could be able to fit ints bigger than
     2^32 - 1, which would allow for more valid integral perfect
     numbers.
***)


(*>* Problem 3f *>*)

let rec interleave (n:int) (l:int list) : int list list =
  match l with
  | x :: xs -> (n :: x :: xs) :: List.map (fun l -> x :: l) (interleave n xs)
  | [] -> [[n]]

let rec permutations (l:int list) : int list list =
  match l with
  | x :: xs -> List.concat (List.map (interleave x) (permutations xs))
  | [] -> [[]]
;;
