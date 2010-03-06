(* CS51 Section 2 *)
(* Week of 2/14/10 *)

(* ************************ Part 1 - 42 ****************************** *)
(* The purpose of these exercises is to help you develop a more formal
 * model of how ocaml code evaluates.  This is useful both when writing
 * and reading programs.  For some relevant examples, see the entries
 * at the Underhanded C contest:
 * http://underhanded.xcott.com/?page_id=5 
 *)

(* For each of these, replace ??? with something that will make the
 * snippet evaluate to the integer 42.  If this is not possible,
 * justify why not.  "I couldn't figure out how" isn't a valid
 * justification.  "No matter what you replace ??? with, this
 * expression cannot possibly evaluate to 42 because..."  is a good
 * start.  *)


let reduce f u xs = List.fold_right f xs u;;

reduce (fun x r -> r * 10 + x) 0 [2; 4];;

(* 42.1 *)
let f = ??? in
  f (42, 24);;



(* 42.2 *)
let x = ??? in
  let x' = (fun x -> let x = x * 2 in x) in
	  reduce (+) 0 (List.filter (fun x -> x = x) [x' x; x])
		;;



(* 42.3 *)
let f = (fun x y -> x + y) in
  let g = f ??? in
	  g 21
		;;



(* 42.4 *)
let f = (fun (x,y) -> x + y) in
  let g = f ??? in
	  g 21
		;;




(* 42.5 *)
let f = (fun (x,y) -> x + y) in
  let g = f (???) in
	  g 21
		;;




(* 42.6 *)
let f = ??? in
   f f (f f) ;;




(* From now on, your definitions cannot contain the value
 * "42"! *)

(* 42.7 *)
let f = ??? in
 reduce f 21 [f] ;;




(* 42.x: Bonus *)
let thequestion = ??? in
  6 * thequestion
	;;

(* ********************* Part 2 - Map/Reduce ******************** *)
(* For more practice writing and using higher order functions. *)


(* 3a. filtermap Write a function that takes
 *    -> a predicate, pred
 *    -> a one argument function f with argument type 'a
 *    -> a list of ('a)s, lst
 * The function should: filter out items that make pred false, and
 * return the result of applying f on each element of the remaining
 * list.
 *
 * Your solution should use reduce.
 *)
let filtermap (pred: 'a -> bool) (f: 'a -> 'b) (lst: 'a list) : 'b list =

   
(* 3b.  Use filtermap to write the deoptionalize function from PS2. 
   As a reminder:
   deoptionalize [None; Some 2; None; Some 3; Some 4; None] = [2;3;4] *)
let deoptionalize = ;;
   

(* You may have noticed that you needed to raise an exception to make
   deoptionalize work properly with arbitrary option types.  Here is
   an alternative way to define filter_map that avoids that
   problem. Try filling in the code for this definition (use reduce
   here too) *)

let filter_map (f: 'a -> 'b option) (lst: 'a list) : 'b list = 
;;

(* Now write deoptionalize using this new filter_map *)
let deoptionalize' = 

(* ********************** Part 3 - Clean it up. ******************** *)

(* Over the course of your programming career, you will undoubtedly encounter
 * some poorly written code that is hard to read, hard to reason about, and 
 * hard to modify.  With high probability, some of it will be your own code 
 * that you're returning to six months after writing it :)  This exercise
 * is here to practice rewriting code to make it better, while at the same time
 * making sure that your new code has the same behavior as the old.
 *)


(* John Hacker recently learned OCaml, but has not taken CS51, and so
 * he doesn't know about map, reduce or proper style. As a result, he
 * isn't aware that this horribly convoluted function he invented can 
 * be written in one short, elegant line of code.
 *
 * Write a function that behaves identically but is simpler and written with
 * correct style. *)
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
 
let mystery' = raise (Failure "Not implemented")








(* ************************ Part 4 - Data type design ******************
 * One of the goals of CS51 is to teach you how to design your own 
 * abstractions. Since you now know how to declare and use algebraic
 * data types, we're now going to get some practice using them to represent
 * non-trivial data structures.  In Real Life, there are often many possible 
 * designs that will all "work".  However, some are much easier to work with
 * than others.  Those are the ones you want to create when you can.
 *)

(*
XML (Extensible Markup Language) is a set of rules for structuring the 
information in a document.  Both XML and HTML are markup languages used 
extensively over the 
Internet. Unlike HTML, which is geared toward displaying data, XML is geared 
toward exchanging data. Another difference is that HTML tags are pre-defined, 
whereas the author of an XML document defines the set of tags. 

Consider the following example XML representing the dining options available 
to undergraduates:


<menu meal="breakfast" day="Saturday">
    <item>

        <name>Scrambled Eggs</name>
        <portion>4 oz</portion>
        <comment>This item is <img src="vegetarian.jpg" /> vegetarian.</comment>

    </item>
    <item>

        <name>Chocolate Chip Pancakes</name>

        <portion>2 each</portion> 
        <comment>Only on Saturdays!</comment>

    </item>
    <item>

        (...)

    </item>

</menu>

A tag is a form of markup that is enclosed within angle brackets. There are 
three types of tags: start-tags (e.g. <item>), end-tags (e.g. </item>), and 
empty-element tags (e.g. <img src="vegetarian.jpg" />). 

An attribute is a name/value pair associated with a start-tag or empty-element 
tag. For instance, the menu tag has an attribute meal with value "breakfast", 
and the img tag has an attribute src with value "vegetarian.jpg". 


An XML document consists of elements. An element is either (i) a start tag, 
followed by content, followed by a matching end tag, or (ii) an empty-element 
tag. Content between start and end tags may include text and other elements.


A well-formed XML document has matching start- and end-tags. Moreover, start- 
and end-tags nest properly, so an element such as 
<item><name>Scrambled Eggs</item></name> would be illegal.

Write down a datatype that describes all XML documents possible under this 
description. Your datatype should ensure a well-formed XML document--it 
should be impossible to have an unpaired start-tag, an unpaired end-tag or 
improperly nested elements.


Note: If you find it necessary to simultaneously define two types, use the 
"and" operator, as in the following example:
  

type foo = Bar of bar | String of string 
and
  bar = Foo of foo | Int of int;; *)

(* YOUR DEFINITION HERE *)












(* Part b: Using your datatype, write down the ocaml expression that represents the following xml. 
<menu meal="breakfast" day="Saturday">
    <item>
        <name>Eggs</name>
        <portion>4 oz</portion>
        <comment>This item is <img src="vegetarian.jpg" /> vegetarian.</comment>
    </item>
</menu>
    *)
