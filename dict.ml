(* Interfaces and implementations of dictionaries.  A dictionary
 * is used to associate a value with a key.  In our case, we will
 * be using a dictionary to build an index for the web, associating
 * a set of URLs with each word that we find as we crawl the web.
 *)
module type DICT = 
  sig
    type key   
    type value 
    type dict
    val empty : dict 
    val insert : dict -> key -> value -> dict
    val lookup : dict -> key -> value option
    val remove : dict -> key -> dict
    val member : dict -> key -> bool
    val choose : dict -> (key * value * dict) option
    val fold : (key -> value -> 'a -> 'a) -> 'a -> dict -> 'a
  end

(* Arguments to the AssocListDict functor *)
module type DICT_ARG = 
  sig
    type key
    type value
    val compare : key -> key -> Order.order
  end

(* An association list implementation of dictionaries.  *)
module AssocListDict(D:DICT_ARG) : (DICT with type key = D.key
                                         with type value = D.value) = 
  struct
    open Order;;
    type key = D.key;;
    type value = D.value;;
    (* invariant: sorted by key, no duplicates *)
    type dict = (key * value) list;;
    let empty = [] ;;
    let rec insert d k v = 
      match d with 
        | [] -> [(k,v)]
        | (k1,v1)::d1 -> (match D.compare k k1 with 
                            | Less -> (k,v)::d
                            | Eq -> (k,v)::d1
                            | Greater -> (k1,v1)::(insert d1 k v))
    ;;
    let rec lookup d k = 
      match d with 
        | [] -> None
        | (k1,v1)::d1 -> (match D.compare k k1 with
                            | Eq -> Some v1
                            | Greater -> lookup d1 k 
                            | _ -> None)
    ;;
    let member d k = match lookup d k with | None -> false | Some _ -> true
    ;;
    let rec remove d k = 
      match d with 
        | [] -> []
        | (k1,v1)::d1 -> (match D.compare k k1 with 
                            | Eq -> d1
                            | Greater -> (k1,v1)::(remove d1 k)
                            | _ -> d)
    ;;
    let choose d = 
      match d with 
        | [] -> None
        | (k,v)::rest -> Some(k,v,rest)
    ;;
    let fold f e = List.fold_left (fun a (k,v) -> f k v a) e 
    ;;
  end    

(******************************************************************)
(* TODO:  implement a new functor, RBTreeDict that uses red-black *)
(* trees to build dictionaries.  Then replace the use of          *)
(* AssocListDict in moogle.ml with your RBTreeDict.               *)
(******************************************************************)
module RBTreeDict(D:DICT_ARG) : (DICT with type key = D.key
                                      with type value = D.value) = 
  struct
    open Order
    type key = D.key
    type value = D.value 

		type color = Red | Black
    type dict = Leaf | Node of dict * (key * value * color) * dict ;;

    exception ImplementMe ;;

		let rotate_left (d:dict) : dict =
			match d with
				| Leaf -> Leaf
				| Node(Leaf, (k, v, c), Leaf) -> raise ImplementMe
				| Node(Leaf, (k, v, c), r) -> raise ImplementMe
				| Node(l, (k, v, c), Leaf) -> raise ImplementMe
				| Node(l, (k, v, c), r) -> raise ImplementMe
		
    let empty : dict =
			Leaf
    ;;

    let rec insert (d:dict) (k:key) (v:value) : dict = 
      match d with
				| Leaf -> Node(Leaf, (k, v, Red), Leaf)
				| Node (l, (k2, v2, c), r) -> 
						match D.compare k k2 with
							| Eq -> d
							| Less -> Node(insert l k v, (k2, v2, c), r)
							| Greater -> Node(l, (k2, v2, c), insert r k v)
    ;;

    let lookup (d:dict) (k:key) : value option = 
      raise ImplementMe
    ;;

    let member (d:dict) (k:key) : bool = 
      raise ImplementMe
    ;;

    let remove (d:dict) (k:key) : dict = 
      raise ImplementMe
    ;;

    let choose (d:dict) : (key*value*dict) option = 
      raise ImplementMe
    ;;

    let fold (f:key -> value -> 'a -> 'a) (u:'a) (d:dict) : 'a = 
      raise ImplementMe
    ;;
  end
