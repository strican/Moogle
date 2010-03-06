(* CSCI E-250 Spring 2010
 * PS 0 *)

(* Please define these variables with appropriate values *)

let name : (string * string) = ("FIRST", "LAST");;

let fas_username : string = "My FAS account (and forums.seas/cs51) username";;

let num_past_classes : int = -1;;

let num_other_current_classes : int = -1;;

let degree_candidate : bool = false;;

type attendance = In_Person | Livecast | Other of string;;

let attending_section : attendance = Other "I don't know yet";;

let experience : string * string = 
  ("My CS background is CSCI E-50a, and many hours playing Counterstrike.",
   "My math background is a course in Hyperspatial Topology");;

let probability_of_taking_cscie250 : float = 0.75;;

let why_cscie250 : string = "Ocaml is awesome!";;

let something_about_myself : string = "I once wrestled a bear into a shark";;

(* Put these languages in order of comfort, most preferred first.
 * You can remove languages you've never worked with *)

let languages = [
  "C";
  "C++";
  "Java";
  "Python";
  "Ruby";
  "LISP / Scheme";
  "Ocaml";
  "Haskell";
  "Classical Latin";
];;
  

(* **** You shouldn't change anything below this line **** *)

let print = Printf.printf;;

let print_survey = 
  let (first, last) = name in
  let string_attending = 
    (match attending_section with
       | In_Person -> "in person"
       | Livecast -> "via live video stream"
       | Other s -> "Other: " ^ s
    ) in
    (print "----------------------------------------\n";
     print "Name: %s %s\n\n" first last;
     print "FAS account: %s@fas.harvard.edu\n\n" fas_username;
	 print "forums.seas/cs51 forum name: %s\n\n" fas_username;
	 if degree_candidate then print "I am an Extension School degree candidate.\n\n"
	 else print "I am not an Extension School degree candidate.\n\n";
     print "I've taken %d Extension School courses.\n\n" num_past_classes;
	 print "I'm taking %d other courses now.\n\n" num_other_current_classes;
	 print "I'll be attending Wednesday night sections %s\n\n" string_attending;
     print "CS Experience: %s\n\n" (fst experience);
     print "Math Experience: %s\n\n" (snd experience);
	 print "I want to take CSCI E-250 because %s\n\n" why_cscie250;
     print "P(taking CSCI E-250) = %.2f\n\n" probability_of_taking_cscie250;
     print "Here's something interesting about me: %s\n\n" something_about_myself;
     print "Languages in order of preference: %s\n\n" (String.concat "; " languages);
     print "----------------------------------------\n\n";);;


print_survey;;

(* type "make" to compile the file.
  type ./survey to run the program and print the output.  
  Make sure all the values look right.  If they do, submit and 
  you're done! *)
