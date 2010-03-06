(* Section 0: Intro to ML 
 * Exercises *)

(* Return the head of a list, or None if empty. *)

let head (x:int list) : int option =
  (* Your code here *) ;;

(* Return the tail of a list, or None if empty. *)

let tail (x:int list) : int list option =
  (* Your code here *) ;;

(* Return the last int of an int list, or None if empty. *)

let last_number (x:int list) : int option =
  (* Your code here *) ;;

(* Square all the elements of a list. *)

let rec square_all (a:int list) : int list =
  (* Your code here *) ;;

(* Return the volume of a cylinder with height h, radius r. *)

let volume_cylinder (h:float) (r:float) : float =
  (* Your code here *) ;;

(* Retain only even integers *)

let rec filter_even (l:int list) : int list =
  (* Your code here *) ;;

(* Zip three lists. Return None if different lengths. *)

let rec threezip (a:int list) (b:int list) (c:int list) :
    ((int * int * int) list) option =
  (* Your code here *) ;;

(* Return the max of a list, or None if the list is empty. *)
(* Note: Might be good to walk through this in English before syntactifying *)

let rec max_of_list (x:int list) : int option =
  (* Your code here *) ;;

(* Return the min and max of a list, or None if the list is empty. *)

let rec bounds (x:int list) : (int * int) option =
  (* Your code here *) ;;

(* From a list of pairs, retain only those that are in order. *)

let rec proper_pairs (l:(int * int) list) : (int * int) list =
  (* Your code here *) ;;

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
  (* Your code here *) ;;

let rec dotproduct (a:int list) (b:int list) : int option =
  (* Your code here *) ;;

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
 * For now, don't worry about doing anything smart if the input 
 * isn't a valid matrix. 
 *)

let rec split (m:int list list) : (int list * int list list) option =
  (* Your code here *) ;;

let rec transpose (m:int list list) : int list list =
  (* Your code here *) ;;
