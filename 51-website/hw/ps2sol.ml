(***GENERAL COMMENTS***

1. Filter_odd on negative numbers: Due to the way the "mod" function works, 
if people attempted to write something like (x mod 2 = 1) in filter_odd, 
their code likely broke on negative numbers. This is an odd case (no pun 
intended), but a perfect example of a bug that should be caught by writing 
good tests. If mult_odds was written in terms of filter_odd, we didn't mark 
off for this bug twice.

2. a. Don't rewrite perfectly good functions. ex. (fun x y -> x + y) is 
better written as (+). That said, if we ask you directly to code a function 
with behavior identical to a library function, don't call it in your solution.
Several people wrote "let concat = List.concat," which, while demonstrating 
effective code reuse, missed the point.

2.b. Same thing with functions you've already written in the same file. 
Several people rewrote "sum" in a few places rather than simply calling it.

2.c. Don't wrap perfectly good functions in funs. "(fun x -> sum x)" behaves 
exactly identically to "sum", and both can be passed as arguments.

3. "if (boolean expression) then true else false" is better written as 
"(boolean expression)." "if (boolean expression) then false else true" is 
better written as "not (boolean expression)"

4. Some people wrote various functions recursively instead of using map or 
reduce. This isn't what we wanted, and in order to do it, you had
to change a function declaration we gave you, which you shouldn't do.

5. Don't match when there is only one possibility; use let!
"let (x,y) = 2tuple in x + y" is preferable to "match 2tuple with
| (x,y) -> x + y"
6. x::tl NOT [x] @ tl
7. Write f x NOT f(x). It's not C!
8. (~-) is negation
9. Assignment and comparison both use =, NOT ==.
***)

(* This function should prove useful *)
let rec reduce (f:'a -> 'b -> 'b) (u:'b) (xs:'a list) : 'b =
  match xs with
    | [] -> u
    | hd::tl -> f hd (reduce f u tl) ;;



(***********************************************)
(******            1.1 WARM UP            ******)
(***********************************************)

(*>* Problem 1.1.a *>*)

(* Exercise 0.  Implement reduce using List.fold_right *)
let reduce_mine (f:'a -> 'b -> 'b) (u:'b) (xs:'a list) : 'b =
  List.fold_right f xs u
;;



(****************************************************)
(******       1.2: Sparking your INTerest      ******)
(****************************************************)

(*>* Problem 1.2.a *>*)

(* a. negate_all: Flips the sign of each element in a list *)
let negate_all (nums:int list) : int list =
  List.map (fun n -> -n) nums
;;

(* Unit test example.  Uncomment after writing negate_All *)
(* assert ((negate_all [1; -2; 0]) = [-1; 2; 0]);; *)


(*>* Problem 1.2.b *>*)

(* b. sum: Implement sum using reduce! *)
let sum (nums:int list) : int =
  reduce (+) 0 nums
;;


(*>* Problem 1.2.c *>*)

(* c. sum_rows: Sums each list of integers in a list of lists of integers *)
let sum_rows (rows:int list list) : int list =
  List.map sum rows
;;


(*>* Problem 1.2.d *>*)

(* d. filter_odd: Retains only the odd numbers from the given list *)
let filter_odd (nums:int list) : int list =
  List.filter (fun n -> n mod 2 <> 0) nums
;;
(*** There were various ways of doing this. See comment above. ***)

(*>* Problem 1.2.e *>*)

(* e. num_occurs: Returns the number of times a given number (n)
 *                appears in a list (nums) *)
let num_occurs (nums:int list) (n:int) : int =
  reduce (fun n' count -> (if n' = n then 1 else 0) + count) 0 nums
;;


(*>* Problem 1.2.f *>*)

(* f. super_sum: Sums all of the numbers in a list of int lists *)
let super_sum (nlists:int list list) : int =
  reduce (fun cur_list working_sum -> (sum cur_list) + working_sum) 0 nlists
;;

(*>* Problem 1.2.g *>*)

(* g. filter_range: Returns a list of numbers in the input list within a
 *                  given range (inclusive), in the same order they appeared
 *                  in the input list.
 * For example, filter_range [1;3;4;5;2] (1,3) = [1;3;2] *)
let filter_range (nums:int list) (range:int * int) : int list =
  let (low, high) = range in
  List.filter (fun n -> (n >= low) && (n <= high)) nums
;;



(****************************************************)
(**********       1.3 Fun with Types       **********)
(****************************************************)

(*>* Problem 1.3.a *>*)

(* a. floats_of_ints: Converts an int list into a list of floats *)
let floats_of_ints = List.map float_of_int ;;

(* Note we have dropped the (nums: int list) argument from the function
 * signature because, by currying, floats_of_ints returns a function
 * that takes in an int list. Stylistically, it is preferable to drop
 * arguments when currying makes them unnecessary, and this can be done
 * for other solutions in this problem set. *)

(*>* Problem 1.3.b *>*)

(* b. optionalize: Converts each int in an list to an int option *)

let optionalize (lst: 'a list) : 'a option list =
  List.map (fun x -> Some x) lst
;;

(*** Many people wrote a helper function for parts c and d. While factoring
 *** out code is good, this brings up the problem of what to do on the
 *** None case. If you wanted to use a helper function, the best thing
 *** was probably to simply write a working version of deoptionalize above
 *** some_sum and call it later. ***)
(*>* Problem 1.3.c *>*)

(* c. some_sum: Sums all of the numbers in a list of int options;
 *              ignores None values *)
let some_sum (nums:int option list) : int =
  reduce (fun opt accum ->
                (match opt with Some x -> x | None -> 0) + accum) 0 nums 
;;


(*>* Problem 1.3.d *>*)

(* d. deoptionalize: Extracts values from a list of options.
 * Ex: deoptionalize [Some 3; None; Some 5; Some 10] = [3;5;10] *)
let deoptionalize (l:'a option list) : 'a list =
  reduce (fun cur accum ->
               (match cur with None -> accum | Some x -> x::accum)) [] l
;;


(*>* Problem 1.3.e *>*)

(* e. mult_odds: Product of all of the odd members of a list.
 * Ex: mult_odds [1;3;0;2;5] = 15 *)
let mult_odds (nums:int list) : int =
  reduce ( * ) 1 (filter_odd nums)
;;


(*>* Problem 1.3.f *>*)

(* f. concat: Concatenates a list of lists. See the Ocaml library ref *)
let concat (lists:'a list list) : 'a list =
    reduce List.append [] lists
;;


(*>* Problem 1.3.g *>*)

(* the student's name and year *)
type name = string
type year = int
type student = name * year

(* g. filter_by_year: returns the names of the students in a given year
 * Ex: let students = [("Joe",2010);("Bob",2010);("Tom",2013)];;
 *     filter_by_year students 2010 => ["Joe","Bob"] *)
let filter_by_year (slist:student list) (yr:year) : name list =
  reduce (fun cur_student names ->
           let (cur_name, cur_year) = cur_student in
           if cur_year = yr then cur_name::names else names)
         [] slist
                 
;;


(*>* Problem 1.4 *>*)

(* Please give us an honest estimate of how long this Part of the problem
 * set took you to complete.  We care about your responses and will use
 * them to help guide us in creating future assignments. *)
let minutes_spent_on_part_1 : int = 42 ;;

exception ParseError of string


(* ------ Type definitions for the abstract syntax tree ------- *)

(* Binary operators. *)
type binop = Add | Sub | Mul | Div | Pow ;;

(* Unary operators. *)
type unop = Sin | Cos | Ln | Neg ;;

type expression =
  | Num of float
  | Var
  | Binop of binop * expression * expression
  | Unop of unop * expression
;;

type token =
  | NumT of float
  | VarT
  | BinopT of binop
  | UnopT of unop
  | LParen
  | RParen
  | LBrace
  | RBrace
  | EOF
;;

(* ----- Library code -- you shouldn't have to do anything ----- *)

let recognized_tokens = [|"x"; "sin"; "cos"; "ln"|] ;;

let token_expressions = [|VarT; UnopT Sin; UnopT Cos; UnopT Ln|] ;;

let string_to_char_list (s:string) : char list =
  let rec string_to_char_list' (s:string) (acc:char list) (i:int) =
    if i < 0 then acc else
      let c = String.get s i in
	string_to_char_list' s (c::acc) (i-1)
  in string_to_char_list' s [] (String.length s - 1)
;;

let is_digit (c:char) : bool =
  let i = Char.code c in
    i >= 48 && i <= 57
;;

(* The precedence of a binary operator.  Used in the parse_string and
      to_string_smart functions. *)
let binop_precedence (b:binop) : int =
  match b with
    | Add -> 3
    | Sub -> 3
    | Mul -> 2
    | Div -> 2
    | Pow -> 1
;;

let unop_precedence (u:unop) : int = 4 ;;

(* A strict upper bound on the precedence of any operator. *)
let prec_bound : int = 5 ;;

let binop_is_associative (b:binop) : bool =
  match b with
    | Add | Mul -> true
    | Sub | Div | Pow -> false ;;

(* Pretty-printing functions for expressions *)

let binop_to_string (b:binop) : string =
  match b with
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Pow -> "^"
;;

let unop_to_string (u:unop) : string =
  match u with
    | Sin -> "sin"
    | Cos -> "cos"
    | Ln -> "ln"
    | Neg -> "~"
;;

let token_to_string (t:token) : string =
  match t with
    | NumT n -> string_of_float n
    | VarT -> "x"
    | BinopT b -> binop_to_string b
    | UnopT u -> unop_to_string u
    | LParen -> "("
    | RParen -> ")"
    | LBrace -> "{"
    | RBrace -> "}"
    | EOF -> "EOF"
;;

(* Only adds parentheses when needed to prevent ambiguity. *)
let to_string_smart (e:expression) : string =
  let rec to_string_smart' e parent_precedence parent_associative =
    match e with
      | Num n ->
	  if n >= 0.0 then string_of_float n
	  else "~" ^ string_of_float (abs_float n)
      | Var -> "x"
      | Unop (u,e1) ->
	  unop_to_string u ^ "(" ^
	    to_string_smart' e1 (unop_precedence u) false ^ ")"
      | Binop (b,e1,e2) ->
	  let prec = binop_precedence b in
          let e_str = 
	      (to_string_smart' e1 prec false ^
	       binop_to_string b ^
	       to_string_smart' e2 prec (binop_is_associative b)) in
            if prec > parent_precedence ||
                  (prec = parent_precedence && not parent_associative)
            then "(" ^ e_str ^ ")"
	    else e_str
  in to_string_smart' e prec_bound false
;;

(* Always adds parentheses around all binary ops. Completely unambiguous;
       however, often very hard to read... *)
let rec to_string (e:expression) : string =
  match e with
    | Num n ->
	if n >= 0.0 then string_of_float n
        else "~" ^ string_of_float (abs_float n)
    | Var -> "x"
    | Unop (u,e1) -> "(" ^ unop_to_string u ^ "(" ^ to_string e1 ^ "))"
    | Binop (b,e1,e2) -> 
        "(" ^ to_string e1 ^ binop_to_string b ^ to_string e2 ^ ")"
;;

(* Lexing functions (producing tokens from char lists) *)

let rec match_while (p:char -> bool) (l:char list) : string * char list =
  match l with
    | [] -> ("", [])
    | c::cs ->
	if p c then 
	  let (s_cs, l_cs) = match_while p cs in (String.make 1 c ^ s_cs, l_cs)
	else ("", l) ;;

let lex_number_string = match_while (fun c -> is_digit c || c = '.')

let rec lex_number (l:char list) : (token * char list) option =
  let (s,l') = lex_number_string l in
    try Some (NumT (float_of_string s), l')
    with Failure _ -> None ;;

let rec match_string (l:char list) (s:string) : char list option =
  if s = "" then Some l else
    match l with
      | [] -> None
      | h::t ->
	  if h = String.get s 0 then
            match_string t (String.sub s 1 (String.length s - 1))
          else None ;;

let lex_multi_char_token (l:char list) : (token * char list) option  =
  let rec lex_multi_char_token' l i =
    if i >= Array.length recognized_tokens then None
    else
      match match_string l (Array.get recognized_tokens i) with
	| Some l' -> Some (Array.get token_expressions i, l')
	| None -> lex_multi_char_token' l (i+1)
  in lex_multi_char_token' l 0 ;;

let rec lex' (l:char list) : token list =
  match l with
    | [] -> []
    | ' '::cs -> lex' cs
    | c::cs ->
	let (token, l') =
	  (match c with
	   | '+' -> (BinopT Add, cs)
	   | '-' -> (BinopT Sub, cs)
	   | '*' -> (BinopT Mul, cs)
	   | '/' -> (BinopT Div, cs)
	   | '^' -> (BinopT Pow, cs)
	   | '~' -> (UnopT Neg, cs)
	   | '(' -> (LParen, cs)
	   | ')' -> (RParen, cs)
	   | '{' -> (LBrace, cs)
	   | '}' -> (RBrace, cs)
	   | _ ->
	       (match lex_number l with
		| Some (t, l') -> (t, l')
		| None ->
		    (match lex_multi_char_token l with
		     | Some (t, l') -> (t, l')
		     | None -> raise (ParseError "Unrecognized token"))))
	in token :: lex' l' ;;
		    
let lex (s:string) : token list =
  lex' (string_to_char_list s) @ [EOF] ;;

let parse (s:string) : expression =
  let rec parse_toplevel_expression (l:token list) : expression =
    let (e,_,_) = parse_delimited_expression l EOF prec_bound in e

  and parse_expression (l:token list) : expression * token list =
    match l with
      | [] -> raise (ParseError "Unexpected end of string")
      | t::ts ->
          match t with
            | LParen ->
		let (e,l',_) =
		  parse_delimited_expression ts RParen prec_bound in
		  (e,l')
            | RParen -> raise (ParseError "Unexpected rparen")
            | LBrace ->
		let (e,l',_) =
		  parse_delimited_expression ts RBrace prec_bound in
		  (e,l')
            | RBrace -> raise (ParseError "Unexpected rbrace")
            | UnopT u -> parse_unop ts u
            | VarT -> (Var, ts)
            | EOF -> raise (ParseError "Unexpected EOF")
            | NumT n -> (Num n, ts)
            | BinopT b ->
		raise (ParseError ("Unexpected Binop: " ^ token_to_string t))

  and parse_binop (l:token list) (delim:token) (current_prec:int) eq 
      : expression * token list * bool =
    match l with
      | [] -> raise (ParseError "Unexpected end of string 2")
      | t::ts ->
          if t = delim then (eq,ts,true) else
            match t with
              | BinopT b ->
                  let prec = binop_precedence b in
                    if current_prec <= prec then (eq,l,false)
                    else
		      let (eq2,l',d) =
                        parse_delimited_expression ts delim prec in
			if d then (Binop(b,eq,eq2),l',true)
			else parse_binop l' delim current_prec
                          (Binop(b,eq,eq2))
              | _ ->
		  raise
		    (ParseError
                       ("Expecting Binop, but found: " ^ token_to_string t))

  and parse_delimited_expression (l:token list) (delim:token)
      (current_prec:int) : expression * token list * bool =
    match l with
      | [] -> raise (ParseError "Unexpected end of string 3")
      | t::ts ->
          if t = delim then
            raise (ParseError ("Unexpected delim: " ^ token_to_string delim))
          else
	    let (eq,l') = parse_expression l in
              parse_binop l' delim current_prec eq
		
  and parse_unop (tokens:token list) (u:unop) =
    let (e,t) = parse_expression tokens in (Unop(u,e),t)

  in parse_toplevel_expression (lex s)
;;

(* ----- End of library code -- your code starts here ----- *)

(*>* Problem 2.1 *>*)

let rec contains_var (e:expression) : bool =
  match e with
    | Var -> true
    | Unop (_,e1) -> contains_var e1
    | Binop (_,e1,e2) -> contains_var e1 || contains_var e2
    | _ -> false
;;


(*>* Problem 2.2 *>*)

(* --------------------- Evaluate ---------------------- *)

(* The "denotation" of a unary operator.
   We map object-language unary operators to Caml unary operators
   (i.e., functions). *)
let unop_denote (u:unop) =
  match u with
    | Sin -> sin
    | Cos -> cos
    | Ln -> log
    | Neg -> fun x -> -. x  (* We need to wrap this in a fun since otherwise
			       the -. syntax is interpreted as binary minus
			       (i.e. subtraction rather than negation). *)
;;

(* The "denotation" of a binary operator.
   We map object-language binary operators to Caml binary operators
   (i.e., functions). *)
let binop_denote (b:binop) =
  match b with
    | Add -> ( +. )
    | Sub -> ( -. )
    | Mul -> ( *. )
    | Div -> ( /. )
    | Pow -> ( ** )
;;

(* Evaluate expression e at x.
   0/0 is evaluated rather arbitrarily as 0.
   So are 0*nan and 0*infinity. *)

let rec evaluate (e:expression) (x:float) : float =
  match e with
    | Num n -> n
    | Var -> x
    | Unop (u,e1) -> unop_denote u (evaluate e1 x)
    | Binop (b,e1,e2) -> binop_denote b (evaluate e1 x) (evaluate e2 x)
;;


(*>* Problem 2.3 *>*)

let rec derivative (e:expression) : expression =
  match e with
    | Num _ -> Num 0.
    | Var -> Num 1.
    | Unop (u,e1) ->
        (match u with
           | Sin -> Binop(Mul,Unop(Cos,e1),derivative e1)
           | Cos -> Binop(Mul,Unop(Neg,Unop(Sin,e1)),derivative e1)
           | Ln -> Binop(Div,derivative e1,e1)
           | Neg -> Unop(Neg,derivative e1)
	)
    | Binop (b,e1,e2) ->
        match b with
          | Add -> Binop(Add,derivative e1,derivative e2)
          | Sub -> Binop(Sub,derivative e1,derivative e2)
          | Mul -> Binop(Add,Binop(Mul,e1,derivative e2),
                         Binop(Mul,derivative e1,e2))
          | Div -> Binop(Div,
			 Binop(Sub,Binop(Mul,derivative e1,e2),
			       Binop(Mul,e1,derivative e2)),
			 Binop(Pow,e2,Num(2.)))
          | Pow ->
	      if contains_var e2 then
                Binop(Mul,e,Binop(Add,Binop(Mul,derivative e2,Unop(Ln,e1)),
                                  Binop(Div,Binop(Mul,derivative e1,e2),e1)))
              else Binop(Mul,e2,
			 Binop(Mul,derivative e1,Binop(Pow,e1,
                                                       Binop(Sub,e2,Num 1.))))
;;


(*>* Problem 2.4 *>*)

let rec find_zero (e:expression) (guess:float) (epsilon:float) (lim:int)
    : float option =
  let e' = derivative e in
  let rec find_zero' (guess:float) (count:int) : float option =
    if count >= lim then None else
    let e_of_guess = evaluate e guess in
    if abs_float e_of_guess >= epsilon then
      find_zero' (guess -. (e_of_guess /. evaluate e' guess)) (count + 1)
    else Some guess
  in find_zero' guess 0
;;

(*** Make sure that you check the absolute value of your guess.
 *** if you simply check that it's less than epsilon, your code will
 *** not work if the function is negative at a point. ***)

(*>* Problem 2.5 *>*)

let rec simplify (e:expression) : expression =
  match e with
    | Num n -> if (n < 0.) then Unop (Neg, Num (-. n)) else e
    | Var -> e
    | Unop (u,e1) ->
        let e1' = simplify e1 in
          (match u,e1' with
             | Neg, Unop (Neg,e2) -> simplify e2
             | Neg,Num 0. -> Num 0.
             | Ln,Num 1. -> Num 0.
             | Sin,Num 0. -> Num 0.
             | Cos,Num 0. -> Num 1.
             | _ -> Unop(u,e1')
	  )
    | Binop(b,e1,e2) ->
        let e1' = simplify e1 in
        let e2' = simplify e2 in
          match b with
            | Add ->
		if (e1'=e2') then simplify (Binop(Mul,Num 2.,e1')) else
                  (match e1',e2' with
                     | Num 0.,_ -> e2'
                     | _,Num 0. -> e1'
                     | Unop(Neg,e1''),_ -> simplify (Binop(Sub,e2',e1''))
                     | _,Unop(Neg,e2'') -> simplify (Binop(Sub,e1',e2''))
                     | Num n1,Num n2 -> simplify (Num (n1 +. n2))
                     | _ -> Binop(Add,e1',e2')
		  )
            | Sub ->
		if (e1'=e2') then Num 0. else
                  (match e1',e2' with
                     | Num 0.,_ -> simplify (Unop(Neg,e2'))
                     | _,Num 0. -> e1'
                     | _,Unop(Neg,e2'') -> simplify (Binop(Add,e1',e2''))
                     | Unop(Neg,e1''),_ ->
			 simplify (Unop(Neg,Binop(Add,e1'',e2')))
                     | Num n1,Num n2 -> simplify (Num(n1 -. n2))
                     | _ -> Binop(Sub,e1',e2')
                  )
            | Mul ->
		if (e1'=e2') then simplify (Binop(Pow,e1',Num 2.)) else
                  (match e1',e2' with
                     | Num 0.,_ -> Num 0.
                     | _,Num 0. -> Num 0.
                     | Num 1.,_ -> e2'
                     | _,Num 1. -> e1'
                     | Unop(Neg,e1''),_ ->
                         simplify (Unop(Neg,Binop(Mul,e1'',e2')))
                     | _,Unop(Neg,e2'') ->
                         simplify (Unop(Neg,Binop(Mul,e1',e2'')))
                     | Binop(Div,e1'1,e1'2),_ ->
                         simplify (Binop(Div,Binop(Mul,e1'1,e2'),e1'2))
                     | _,Binop(Div,e2'1,e2'2) ->
                         simplify (Binop(Div,Binop(Mul,e1',e2'1),e2'2))
                     | Num n1,Num n2 -> simplify (Num(n1 *. n2))
                     | _ -> Binop(Mul,e1',e2')
                  )
            | Div ->
		if (e1'=e2') then Num 1. else
                  (match e1',e2' with
                     | Num 0.,_ -> Num 0.
                     | _,Num 1. -> e1'
                     | Unop(Neg,e1''),_ ->
                         simplify (Unop(Neg,Binop(Div,e1'',e2')))
                     | _,Unop(Neg,e2'') ->
                         simplify (Unop(Neg,Binop(Div,e1',e2'')))
                     | Binop(Div,e1'1,e1'2),_ ->
                         simplify (Binop(Div,e1'1,Binop(Mul,e1'2,e2')))
                     | _,Binop(Div,e2'1,e2'2) ->
                         simplify (Binop(Div,Binop(Mul,e1',e2'2),e2'1))
                     | Num n1,Num n2 -> simplify (Num(n1 /. n2))
                     | _ -> Binop(Div,e1',e2')
                  )
          | Pow ->
	      (match e1',e2' with
                 | _,Num 0. -> Num 1.
                 | Num 0.,_ -> Num 0.
                 | _,Num 1. -> e1'
                 | _,Unop(Neg,e2'') ->
                     simplify (Binop(Div,Num 1.,Binop(Pow,e1',e2'')))
                 | Binop(Pow,e1'1,e1'2),_ ->
                     simplify (Binop(Pow,e1'1,Binop(Mul,e1'2,e2')))
                 | _ -> Binop(Pow,e1',e2')
              )
;;


(*>* Problem 2.6 *>*)

let minutes_spent_on_part_2 : int = 42 ;;
