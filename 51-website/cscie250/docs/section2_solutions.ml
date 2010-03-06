(* ************************ Part ****************************** *)

let reduce f u xs = List.fold_right f xs u;;

(* 42.1 *)
let f = (fun (x,y) -> x) in
  f (42, 24);;

(* 42.2 *)
(* A good intermediate step would be: *)
let x = ??? in
let true_dat y = true in
let f = (fun a -> let b = a * 2 in b) in
  reduce (+) 0 (List.filter true_dat [f x; x])
		;;
(* answer: *)
let x = 42/3 in
  let x' = (fun x -> let x = x * 2 in x) in
	  reduce (+) 0 (List.filter (fun x -> x = x) [x' x; x])
		;;


(* 42.3 *)
let f = (fun x y -> x + y) in
  let g = f 21 in
	  g 21
		;;

(* 42.4 *)
let f = (fun (x,y) -> x + y) in
  let g = f (1,2) in let g = (fun x->2*x) in
	  g 21
		;;


(* 42.5 *)
let f = (fun (x,y) -> x + y) in
  let g = f (???) in
	  g 21
		;;
(* Impossible -- no way to make the relevant value of g anything but
   an int, so it doesn't typecheck *)

(* 42.6.1 *)
let f = (fun x -> fun x -> 42) in
f f (f f)
;;


(* 42.6.2 *)
(* Trivial if you think correctly about currying, but hopefully
 * is the source of some confusion *)
let f = (fun x y -> fun x -> 42) in
  f f (f f)
;;

(* 42.7 *)
let f = (fun x y -> 2 * y) in
  reduce f 21 [f]
;;

(* 42.x: Bonus *)
let thequestion = 9 in
  6 * thequestion  (* reference: see The Hitchhiker's Guide to the Galaxy, by Douglas Adams *)
	;;

(* ************************ Part ****************************** *)


(*
 * The function should: filter out items that make pred false, and
 * return the result of applying f on each element of the remaining
 * list.
 *)
let filtermap (pred: 'a -> bool) (f: 'a -> 'b) (lst: 'a list) : 'b list = 
    reduce (fun x r -> if pred x then (f x)::r else r) [] lst;;

(* 3b.  Use filtermap to write the deoptionalize function from PS2. As a reminder:
   deoptionalize [None; Some 2; None; Some 3; Some 4; None] = [2;3;4] *)
let deoptionalize lst = filtermap (fun x -> x != None) 
  (fun o -> match o with 
     | Some x -> x
     | None -> raise (Failure("Impossible"))) 
  lst;;

(* You may have noticed that you needed to raise an exception to make
   deoptionalize work properly with arbitrary option types.  Here is
   an alternative way to define filter_map that avoids that
   problem. Try filling in the code (use reduce here too) *)

let filter_map (f: 'a -> 'b option) (lst: 'a list) : 'b list = 
  reduce (fun x r -> match f x with
            | Some a -> a :: r
            | None -> r) [] lst;;


let deoptionalize' lst = filter_map (fun x -> x) lst;;

(* ************************ Part ****************************** *)

(*
 * mystery [] -> []
 * mystery []::tl -> mystery tl
 * mystery [x]::tl -> x :: mystery tl
 * mystery (a::b)::tl -> a :: (mystery (b::tl))
 * So flatten :)
 *)

let rec mystery (lists : 'a list list) =
  if List.length lists = 0 then []
  else if List.length (List.hd lists) = 0 
  then mystery (List.tl lists)
  else if List.length (List.hd lists) = 1
  then let hd = List.hd lists in 
	((List.hd) hd) :: mystery (List.tl lists)
  else let hd = List.hd lists in
	(List.hd) hd :: (mystery ((List.tl hd)::(List.tl lists)))
;;
 
let rec mystery' lists = 
  match lists with
    | [] -> []
    | [] :: tl -> mystery' tl
    | [x] :: tl -> x :: mystery' tl
    | (a::b) :: tl -> a :: (mystery' (b::tl))
  ;;

(* Make sure this behaves the same *)
assert (let x = [[];[]] in mystery x = mystery' x);;
assert (let x = [[1];[1;3;2]] in mystery x = mystery' x);;
assert (let x = [[1;2;4];[1;3;2];[7;8;9]] in mystery x = mystery' x);;

(* Now change to a better name, and make even shorter *)
let flatten lists = reduce (fun lst r -> lst @ r) [] lists;;

(* Make sure this behaves the same too *)
assert (let x = [[];[]] in mystery x = flatten x);;
assert (let x = [[1];[1;3;2]] in mystery x = flatten x);;
assert (let x = [[1;2;4];[1;3;2];[7;8;9]] in mystery x = flatten x);;

(* ************************ Part ****************************** *)

(* xml type def.
 * Note: do not want to allow a toplevel string with no tags. "blah" is not valid xml.
 "<foo>some stuff <bar x="y"/> more stuff</foo>" is. 
 *)

type attribute = {name: string ; value : string};;

type attributes = attribute list;;

type element = EmptyTag of string * attributes 
               | PairedTag of string * attributes * xml_content 
and
  xml_content = String of string 
                     | Element of element 
                     | XmlList of xml_content list;;

type xml_doc = element list;;

(* Definition for:
<menu meal="breakfast" day="Saturday">
    <item>
        <name>Eggs</name>
        <portion>4 oz</portion>
        <comment>This item is <img src="vegetarian.jpg" /> vegetarian.</comment>
    </item>
</menu>
*)

let link = EmptyTag ("img", [{name="src"; value="vegetarian.jpg"}]) in
let veggie_comment = PairedTag ("comment", 
                                [],
                                XmlList ([String "This item is "; 
                                          Element link;
                                         String " vegetarian."])) in
let eggs = PairedTag ("item", [], 
                      XmlList ([Element (PairedTag ("name", [], String "Eggs"));
                                Element (PairedTag ("portion", [], String "4oz"));
                                Element veggie_comment
                               ])) in
let menu = PairedTag ("menu", 
                      [{name="meal"; value="breakfast"}; {name="day";value="Saturday"}],
                      XmlList [Element eggs])
in menu;;
                    
