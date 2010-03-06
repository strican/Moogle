(* Harvard CS51:  Polymorphism and Higher-Order Functions *)

(* increment all integers in a list *)
let rec inc_all (xs:int list) : int list = 
  match xs with 
    | [] -> []
    | hd::tl -> (hd+1)::(inc_all tl) ;;

(* square all integers in a list *)
let rec square_all (xs:int list) : int list = 
  match xs with  
    | [] -> []
    | hd::tl -> (hd*hd)::(square_all tl) ;;

(* apply a function f to each element in a list -- found in List.map *)
let rec map f xs = 
  match xs with
    | [] -> []
    | hd::tl -> (f hd)::(map f tl) ;;

(* Now we can use map to define inc_all and square_all *)
let inc x = x+1 ;;
let inc_all xs = map inc xs ;;
let square y = y*y ;;
let square_all xs = map square xs ;;

(* We can also use anonymous functions to avoid having to give names
 * to the functions we pass to map. *)
let inc_all xs = map (fun x -> x + 1) xs ;;
let square_all xs = map (fun y -> y*y) xs ;;

(* add the elements in a list *)
let rec sum (xs:int list) : int = 
  match xs with
    | [] -> 0
    | hd::tl -> hd + (sum tl) ;;

(* multiply the elements in a list *)
let rec prod (xs:int list) : int = 
  match xs with 
    | [] -> 1
    | hd::tl -> hd * (prod tl) ;;

(* a generic reduction function for lists -- combine the elements
 * using f and use u for the base case -- known as List.fold_right
 * in the library *)
let rec reduce f u xs = 
  match xs with 
    | [] -> u
    | hd::tl -> f hd (reduce f u tl) ;;

(* Now we can use reduce to define sum and prod *)
let add x y = x+y ;;
let mul x y = x*y ;;

let sum xs = reduce add 0 xs ;;
let prod xs = reduce mul 1 xs ;;

(* Or again, we can just use anonymous functions *)
let sum xs = reduce (fun x y -> x+y) 0 xs ;;
let prod xs = reduce (fun x y -> x*y) 1 xs ;;

(* Combining map and reduce:  sum of squares *)
let sum_squares xs = sum (map square xs) ;;

(* Function definitions are actually abbreviations for normal
 * variable declarations, where we bind an anonymous function
 * to the variable.  *)

(* So we can write: *)
let square = (fun x -> x*x) ;;
let add = (fun x y -> x+y) ;;

(* Even though add seems to take two arguments, it's actually
   a function that takes 1 argument (x) and returns a function
   (fun y -> x+y) which when given 1 argument (y), returns
   x+y. *)
let add = (fun x -> (fun y -> x+y)) ;;

(* We can partially apply a Curried function like add *)
let inc = add 1 ;;

(* if we substitute 1 for x in add's definition, we get *)
let inc = (fun y -> 1+y) ;;

(* which is equivalent to *)
let inc y == 1+y ;;

(* We can also use partial applications to simplify some definitions.
   For instance, we can define square_all simply as:
*)
let square_all = map square ;;

(* Since map expects a function f, and then returns a function which,
   when given a list xs, applies f to each element of xs, the definition
   above means that square_all is bound to a function which when given
   xs, computes the square of xs. *)

(* So we can also simplify the definitions of sum and prod *)
let sum = reduce add 0 ;;
let prod = reduce mul 1 ;;

(* In fact, we can pass in operators, like + and * directly if we
 * wrap them in parentheses to tell the parser that these special 
 * functions aren't yet given their arguments. *)
let sum = reduce (+) 0 ;;
let prod = reduce ( *) 1 ;;

(* Notice the space in the definition of prod, which is necessary
 * to keep the parser from thinking we have a comment. *)

(* Functions like map are actually polymorphic *)

let rec map (f:'a->'b) (xs:'a list) : 'b list = 
  match xs with 
    | [] -> []
    | hd::tl -> (f hd)::(map f tl) ;;

(* Here, the type variables 'a and 'b represent arbitrary types -- so
 * we can use map to take int lists to float lists, or string lists to
 * bool lists, etc. *)

(* map used on integers *)
map (fun x -> x + 1) [1;2;3;4] ;;

(* map used on floats *)
map (fun x -> x +. 2.0) [3.1415; 2.718; 42.0] ;;

(* map used on strings *)
map String.uppercase ["greg"; "victor"; "joe"] ;;

(* a function to split a list into two (almost) equal sized lists --
 * we can split lists of integers, lists of floats, lists of 
 * strings, etc. *)
let rec split (xs:'a list) (ys:'a list) (zs:'a list) : 'a list * 'a list =
  match xs with 
    | [] -> (ys, zs)
    | x::rest -> split rest zs (x::ys) ;;

(* A polymorphic merge function -- the lt function is used to
 * compare elements from the two lists.  Parameterizing the
 * merge by this will allow us to select what order we want in
 * the sort routine below. *)
let rec merge (lt:'a->'a->bool) (xs:'a list) (ys:'a list) : 'a list = 
  match (xs,ys) with 
    | ([],_) -> ys
    | (_,[]) -> xs
    | (x::xst, y::yst) -> 
           if lt x y then x::(merge lt xst ys)
           else y::(merge lt xs yst) ;;

let rec mergesort (lt:'a->'a->bool) (xs:'a list) : 'a list = 
  match xs with 
    | ([] | _::[]) -> xs
    | _ -> let (first,second) = split xs [] [] in
         merge lt (mergesort lt first) (mergesort lt second) ;;

mergesort (<) [3;2;7;1] ;;
mergesort (>) [3;2;7;1] ;;

mergesort (<) [2.718; 3.1415; 42.0] ;;
mergesort (>) [2.718; 3.1415; 42.0] ;;

mergesort (fun x y -> String.compare x y < 0)
  ["Victor"; "Greg"; "Joe"] ;;

(* We don't have to pass in the comparison function each time -- 
 * we can use partial applications to specialize the sort *)
let int_sort = mergesort (<) ;;
let int_sort_down = mergesort (>) ;;
let string_sort = mergesort (fun x y -> String.compare x y < 0) ;;

int_sort [3;2;7;1] ;;
int_sort_down [3;2;7;1] ;;
string_sort ["Victor"; "Greg"; "Joe"] ;;

(* The composition function -- this is the grandmother of all higher-order
 * functions. *)
let comp f g x = f (g x) ;;

(* Remember, this is an abbreviation for: *)
let comp = fun f -> (fun g -> (fun x -> f (g x))) ;;

(* We can use comp to glue together two functions as in: *)
let mystery = comp (add 1) square ;;

(* We can use comp to build new functions for us, and optimize
 * traversals: *)
map inc (map inc [1;2;3;4]) ;;

map (comp inc inc) [1;2;3;4] ;;

(* We can define many interesting functions using reduce *)

let rec append xs ys = 
  match xs with
    | [] -> ys
    | x::rest -> x::(append rest ys) ;;

let append xs ys = reduce (fun x y -> x::y) ys xs ;;

append [1;2;3] [4;5;6] ;;

let rec filter_out (p:'a->bool) (xs:'a list) : 'a list = 
  match xs with
    | [] -> []
    | x::rest -> 
        if p x then filter_out p rest
        else x::(filter_out p rest) ;;

let rec filter_out p = 
  reduce (fun x frest -> if p x then frest else x::frest) [] ;;

filter_out (fun x -> x mod 2 = 0) [1;2;3;4;5;6;7;8] ;;

let copy = reduce (fun x y -> x::y) [] ;;

let map f = reduce (fun x y -> (f x)::y) [] ;;



