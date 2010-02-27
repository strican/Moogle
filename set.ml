(* Definitions for sets. *)

(* An interface for set modules *)
module type SET = 
sig
  type elt  (* type of elements in the set *)
  type set  (* abstract type for the set  *)

  (* the empty set *)
  val empty : set
  (* is_empty s returns true iff s is empty *)
  val is_empty : set -> bool
  (* insert an element into the set *)
  val insert : elt -> set -> set
  (* same as insert x empty *)
  val singleton : elt -> set
  (* compute the union of two sets *)
  val union : set -> set -> set
  (* compute the intersection of two sets *)
  val intersect : set -> set -> set
  (* remove an element from the set -- if the
   * element isn't present, does nothing. *)
  val remove : elt -> set -> set
  (* returns true iff the element is in the set *)
  val member : set -> elt -> bool
  (* chooses some member from the set, removes it 
   * and returns that element plus the new set.  
   * If the set is empty, returns None. *)
  val choose : set -> (elt * set) option
  (* fold a function across the elements of the set
   * in some unspecified order. *)
  val fold : (elt -> 'a -> 'a) -> 'a -> set -> 'a
end

(* parameter to Set modules -- we must pass in some 
 * type for the elements of a set, plus a comparison
 * function. 
 *)
module type COMPARABLE = 
  sig
    type t
    val compare : t -> t -> Order.order
  end

(* A simple, list-based implementation of sets. *)
module ListSet(C: COMPARABLE) : (SET with type elt = C.t) = 
  struct
    open Order
    type elt = C.t 
    (* invariant: sorted, no duplicates *)
    type set = elt list
    let empty = []
    let is_empty xs = 
      match xs with 
        | [] -> true
        | _ -> false
    let singleton x = [x]
    let rec insert x xs = 
      match xs with 
        | [] -> [x]
        | y::ys -> (match C.compare x y with 
                      | Greater -> y::(insert x ys)
                      | Eq -> xs
                      | Less -> x::xs)
    let union xs ys = List.fold_right insert xs ys
    let rec remove y xs = 
      match xs with 
        | [] -> []
        | x::xs1 -> (match C.compare y x with 
                      | Eq -> xs1
                      | Less -> xs
                      | Greater -> x::(remove y xs1))
    let rec intersect xs ys = 
      match xs, ys with 
        | [], _ -> []
        | _, [] -> []
        | xh::xt, yh::yt -> (match C.compare xh yh with 
                               | Eq -> xh::(intersect xt yt)
                               | Less -> intersect xt ys
                               | Greater -> intersect xs yt)
    let rec member xs x = 
      match xs with 
        | [] -> false
        | y::ys -> (match C.compare x y with
                      | Eq -> true
                      | Greater -> member ys x
                      | Less -> false)
    let choose xs = 
      match xs with 
        | [] -> None
        | x::rest -> Some (x,rest)
    let fold f e = List.fold_left (fun a x -> f x a) e 
  end

(*****************************************************************)
(* TODO:  implement a new functor, RBTreeSet that uses red-black *)
(* trees to build sets.  Then replace the use of ListSet in      *)
(* moogle.ml with your RBTreeSet.  Hint:  you can implement Sets *)
(* in terms of Dicts and save a lot of work...                   *)
(*****************************************************************)

module DictSet(D : Dict.DICT with type value = unit) 
  : (SET with type elt = D.key) = 
struct

    type elt = D.key
    type set = D.dict

    let empty : set = D.empty;;
    let is_empty (s:set) : bool = (D.choose s = None);;
    let insert (e:elt) (s:set) : set = D.insert s e ();;
    let singleton (e:elt) : set = insert e (empty);;
 

    let remove (e : elt) (s:set) : set = D.remove s e;;
    let member (s:set) (e:elt) : bool = D.member s e;;
    let choose (s:set) : (elt * set) option = match D.choose s with
      | None -> None
      | Some (k,v,d) -> Some (k,d)  ;;

    let fold (f:elt -> 'a -> 'a) (u:'a) (s:set) : 'a = 
			D.fold (fun k _ d -> f k d) u s;;

    let union (s1:set) (s2:set) : set = fold insert s1 s2 ;;
    let intersect (s1:set) (s2:set) : set = 
      fold (fun k s -> if (member s2 k) then insert k s else s) (empty) s1;;

end
