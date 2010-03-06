(* Harvard CS51: Introduction to Ocaml *)

(* find "name" "path":  tries to find all occurrences of files with the
    given name, starting at the given path, recursively exploring all sub-
    directories.  Returns a list of the full paths to the files.
*)
let rec find name path = 
  try 
    let fullname = Filename.concat path name in 
      if Sys.file_exists fullname then
        [ fullname ]
      else if Sys.is_directory path then 
        let files = Array.to_list(Sys.readdir path) in 
        let paths = List.map (Filename.concat path) files in
        let results = List.map (find name) paths in 
          List.flatten results
      else [] 
  with Sys_error _ -> []
;;

(* p is a local variable bound to the pair (1,3) -- we pull
 * apart the pair, binding the first component to x and the
 * second component to y, and then return x+y. *)
let p = (1,3) in 
let (x,y) = p in x+y
;;

(* the float squaring function -- remember that floating point 
 * operations need a "." after the operator. *)
let square x = x *. x 
;;

(* a function for computing the distance between two cartesian points. *)
let distance p1 p2 = 
  let (x1,y1) = p1 in
  let (x2,y2) = p2 in 
    sqrt (square (x2 -. x1) +. square(y2 -. y1)) 
;;

(* an alternative way of writing distance *)
let distance2 p1 p2 = 
  match p1 with 
    | (x1,y1) -> 
        match p2 with 
            (x2,y2) -> 
              sqrt (square (x2 -. x1) +. square(y2 -. y1)) 
;;

(* yet another way of writing distance *)
let distance3 p1 p2 = 
  match (p1, p2) with 
    | ((x1,y1), (x2,y2)) -> 
        sqrt (square (x2 -. x1) +. square(y2 -. y1)) 
;;

(* calculate the slope of a line specified by two cartesian points p1
 * and p2.  Return None if the slope is infinite and Some s where s is
 * is the slope otherwise. 
 *)
let slope p1 p2 = 
  let (x1,y1) = p1 in 
  let (x2,y2) = p2 in 
    if x2 -. x1 = 0.0 then None
    else Some((y2 -. y1) /. (x2 -. x1))
;;

(* a list of strings *)
let family = ["Greg"; "Tanya"; "Amy"; "John"] 
;;

(* abbreviates a bunch of uses of "::" terminated by a "[]" *)
let family2 = "Greg" :: "Tanya" :: "Amy" :: "John" :: []
;;

(* same as above but with explicit parentheses *)
let family3 = "Greg" :: ("Tanya" :: ("Amy" :: ("John" :: [])))
;;

(* return Some of the head of a list if present, None otherwise *)
let head x = 
  match x with 
    | [] -> None
    | hd :: _ -> Some hd
;;

(* compute the product of a list of integers *)
let rec prods (l: (int*int) list) : int list = 
  match l with 
    | [] -> []
    | (x,y) :: tl -> (x*y) :: (prods tl)
;;

(* zip up a pair of lists into a list of pairs.  Return None if
 * the lists are of unequal length. *)
let rec zip (x:int list) (y:int list) : ((int*int) list) option = 
  match (x,y) with 
    | ([], []) -> Some []
    | (xhd::xtl, yhd::ytl) -> 
        (match zip xtl ytl with 
           | None -> None
           | Some ztl -> Some ((xhd,yhd)::ztl))
    | (_, _) -> None
;;

(* A record type definition *)
type employee = {name:string ; age:int ; married:bool} 
;;

(* some example records -- notice field order doesn't matter *)
let g : employee = {name = "Greg"; married = true; age = 112} ;;
let v = {married = false; name = "Victor"; age = 77} ;;
let w = {name = "Gideon"; age = -12; married = false} ;;
let z = {name = "Joe"; age = 0; married = false} ;;
let c = {name = "Chelsea"; age = 3; married = false} ;;
let t = {name = "Tanya"; age = 110; married = true} ;;

(* a list of the employee records *)
let db : employee list = [g;v;w;z;c] ;;

(* calculate the difference in age of two employees *)
let age_diff (e1:employee) (e2:employee) = 
  let {name=n1 ; age=a1; married=m1} = e1 in
  let {name=n2 ; age=a2; married=m2} = e2 in
    a2 - a1 
;;

(* same as above, but omitting unused fields in the patterns *)
let age_diff2 e1 e2 = 
  let {age=a1} = e1 in 
  let {age=a2} = e2 in 
    a2 - a1
;;

(* even better -- use the "." notation *)
let age_diff3 e1 e2 = e2.age - e1.age
;;

(* insert an employee e into an age-sorted list of employees *)
let rec insert (e:employee) (db:employee list) : employee list = 
  match db with 
    | [] -> e::[]
    | dbhd :: dbtl -> 
        if e.age <= dbhd.age then 
          e :: db
        else 
          dbhd :: (insert e dbtl)
;;

(* insert all of the employees in db1 into the age-sorted list of 
 * employees db2. *)
let rec insert_list (db1:employee list) (db2:employee list) : employee list = 
  match db1 with 
    | [] -> db2
    | db1hd :: db1tl -> 
        insert_list db1tl (insert db1hd db2)
;;

(* sort a list of employees based on their age *)
let sort db = insert_list db []
;;

(* an int_list is either Null or Cons(i,x) where i is an int and x is an
 * int_list. *)
type int_list = Null | Cons of int*(int list) ;;

(* a boolean is either True or False -- Ocaml actually defines "real"
 * bools as true and false, which are the only lower-case data 
 * constructors in the language.  *)
type boolean = True | False ;;

type color = Red | Green | Blue ;;

(* this is isomorphic to an int option -- we just used Nothing for
 * None, and Just for Some.  *)
type int_maybe = Nothing | Just of int ;;

(* A more interesting datatype -- a value can be an int, string,
 * bool, or pair of values according to this definition. *)
type value = Int of int | Str of string | Bool of bool | Pair of value*value ;;

(* some example values *)
let v1 = Int 3 ;;
let v2 = Bool true ;;
let v3 = Pair (v1, v2) ;;
let v4 = Pair (Pair (Int 12, Bool false), 
               Pair (v3, Str "moo")) ;;

(* compute the list of all strings occuring in a value *)
let rec all_strings (v:value) : string list = 
  match v with 
    | Int _ -> []
    | Str s -> s::[]
    | Bool _ -> []
    | Pair (v1,v2) -> (all_strings v1) @ (all_strings v2)
;;

(* type specifications for GML *)
type markup = Ital | Bold | Font of string ;;

type elt = Words of string list | Markup of markup * elt ;;

type doc = elt list ;;

(* an example GML document *)
let d = [ Markup (Bold, 
                  Markup (Font "Arial", 
                          Words [ "Chapter" ; "One" ])) ;
          Words ["It"; "was"; "a"; "dark"; "&"; "stormy"; "night."; "A"] ;
          Markup (Ital, Words [ "shot" ]) ;
          Words [ "rang"; "out." ]
        ] 
;;

(* code to change all occurrences of "Arial" to "Courier" *)
let chmarkup(m:markup) : markup = 
  match m with 
    | Font "Arial" -> Font "Courier"
    | (Ital | Bold | Font _) -> m
;;

let rec chfont (e:elt) : elt = 
  match e with 
    | Words ws -> Words ws
    | Markup(m,e) -> Markup(chmarkup m, chfont e)
;;

let rec chfonts (elts:doc) : doc = 
  match elts with 
    | [] -> []
    | ehd :: etl -> (chfont ehd) :: (chfonts etl)
;;
