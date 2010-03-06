(* Section 0: Intro to ML 
 * Solutions to exercises *)

(* Return the head of a list, or None if empty. *)

let head (x:int list) : int option =
  match x with 
  | [] -> None
  | xhd :: xtl -> Some xhd ;;

(* Return the tail of a list, or None if empty. *)

let tail (x:int list) : int list option =
  match x with
  | [] -> None
  | xhd :: xtl -> Some xtl ;;
  
(* Return the last int of an int list, or None if empty. *)

let rec last_number (x:int list) : int option =
  match x with
  | [] -> None
  | xhd :: [] -> Some xhd
  | xhd :: xtl -> last_number xtl ;;

(* Square all the elements of a list. *)

let rec square_all (a:int list) : int list =
  match a with
  | [] -> []
  | ahd :: atl -> (ahd * ahd) :: square_all atl ;;

(* Note here how function application takes precedence over cons
   (and other infix operators). *)

(* Return the volume of a cylinder
 * with height h, radius r. *)

let volume_cylinder (h:float) (r:float) : float =
  let pi = 3.14159 in
    pi *. r *. r *. h ;;

(* Retain only even integers *)

let rec filter_even (l:int list) : int list =
  match l with
   | [] -> []
   | h :: t -> if h mod 2 = 0 then h :: filter_even t
               else filter_even t ;;

(* Zip three lists. Return None if different lengths. *)

let rec threezip (a:int list) (b:int list) (c:int list) :
    ((int * int * int) list) option =
  match (a, b, c) with
  | ([], [], []) -> Some []
  | ([], _, _) -> None
  | (_, [], _) -> None
  | (_, _, []) -> None
  | (ahd :: atl, bhd :: btl, chd :: ctl) -> 
      match threezip atl btl ctl with
      | None -> None
      | Some ntl -> Some ((ahd, bhd, chd) :: ntl) ;;

(* Lots of pattern matches. How can we make this code simpler? *)

let rec threezip_short (a:int list) (b:int list) (c:int list) :
    ((int * int * int) list) option =
  match (a, b, c) with
  | ([], [], []) -> Some []
  | (ahd :: atl, bhd :: btl, chd :: ctl) -> 
      (match threezip_short atl btl ctl with
       | None -> None
       | Some ntl -> Some ((ahd, bhd, chd) :: ntl))
  | (_,_,_) -> None ;;

(* Why is it OK to simplify our code like that?
   Didn't Greg warn us against using the catch-all pattern _ ?
   If we do that, the compiler might not warn us of potential bugs
   when we change our definitions.
     - Maybe it's OK because we're not using _ per se, but rather (_,_,_).
       - But that would cause the same problem in Greg's example,
         if we were matching on a tuple of the Markup type.
     - There's a tradeoff between using _ for succinct pattern-matching
         and using explicit patterns for bug-proofing.
         Generally, it's acceptable to use _ if we can't reasonably expect
         the type being matched to change (in its definition)
         while the function still serves its stated purpose.
         For instance, the list type is baked into the standard library,
         and we couldn't change it if we wanted to (without causing all
         sorts of other things to break).
     - Still, when using _ (prudently), we should try to make our pattern
         as specific as possible while retaining the conciseness win:
         both for bug-proofing (as above) and for readability.
         For instance, it was better for us to write (_,_,_) above
         than to write _, since it reminds the reader what kinds of cases
         we're catching. *)

(* Return the max of a list, or None if the list is empty. *)

let rec max_of_list (x:int list) : int option =
  match x with
  | [] -> None
  | xhd :: xtl ->
    (match max_of_list xtl with
	| None -> Some xhd (* max_of_list xtl is None when xtl is empty; then
                          xhd is the only element, so it's the max element *)
	| Some max -> Some (if xhd > max then xhd else max));;
   
(* Return the min and max of a list, or None if the list is empty. *)

let rec bounds (x:int list) : (int * int) option =
  match x with
  | [] -> None
  | xhd :: xtl -> 
      (match bounds xtl with
      | None -> Some (xhd, xhd)  (* xtl is empty, so xhd is the only element;
			            it's the min and the max of x. *)
      | Some (min, max) ->

        Some ((if xhd < min then xhd else min),
              (if xhd > max then xhd else max))) ;;

(* Note the double parentheses around the elements in the pair:
     ((if xhd < min then . . .
   Some people (especially those who have used other functional languages)
   might be tempted to leave them out.
   However, "," binds tighter than if-then-else.
   Highjinks may ensue. *)

(* From a list of pairs, retain only those that are in order. *)

let rec proper_pairs (l:(int * int) list) : (int * int) list =
  match l with
  | [] -> []
  | (x1, x2) :: ps -> let pps = proper_pairs ps in
                      if x1 <= x2 then ((x1, x2) :: pps) else pps ;;

(* Can also introduce abbreviated syntax: *)
   
let rec proper_pairs (l:(int * int) list) : (int * int) list =
  match l with
  | [] -> []
  | ((x1, x2) as p) :: ps -> let pps = proper_pairs ps in
                             if x1 <= x2 then (p :: pps) else pps ;;

(* Compute the dot product of two lists.
 * Use zip, prods from lecture, write sum. *)

let rec prods (l: (int*int) list) : int list = 
  match l with 
    | [] -> []
    | (x,y) :: tl -> (x*y) :: (prods tl)
;;

let rec zip (x:int list) (y:int list) : ((int*int) list) option = 
  match (x,y) with 
    | ([], []) -> Some []
    | (xhd::xtl, yhd::ytl) -> 
        (match zip xtl ytl with 
           | None -> None
           | Some ztl -> Some ((xhd,yhd)::ztl))
    | (_, _) -> None
;;

let rec sum (l:int list) : int =
  match l with
    | [] -> 0
    | h::t -> h + (sum t);;

let rec dotproduct (a:int list) (b:int list) : int option =
  match zip a b with
    | None -> None  
    | Some v -> Some (sum (prods v));;

(* Given a matrix (list of lists), return the transpose.
 * The transpose of a matrix interchanges the rows and columns.
 * For example, transpose [[1;2;3];[4;5;6]];;
 * where [1;2;3] and [4;5;6] are the rows,
 * should return [[1;4];[2;5];[3;6]].
 * 
 * Hint: write an auxiliary function, split, that 
 * returns the first column of a matrix as a list
 * and the rest of the matrix as a list of rows.
 * 
 * Behavior of solution is rather odd if m is not a valid matrix.  
 *)

let rec split (m:int list list) : (int list * int list list) option =
  match m with
  | [] -> None
  | [] :: ls -> split ls
  | (x :: xs) :: ls ->
      match split ls with
      | Some (c, ls_rest) -> Some (x :: c, xs :: ls_rest)
      | None -> Some ([x], [xs]) ;;

let rec transpose (m:int list list) : int list list =
  match split m with
  | Some (c, m_rest) -> c :: transpose m_rest
  | None -> [] ;;
