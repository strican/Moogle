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
		exception ArgumentError of string ;;
		exception Impossible ;;

		let check_root_color (n : dict) : color = 
			match n with
				| Leaf -> Black
				| Node(_, (_, _, c), _) -> c 
		;;

		let change_color (c : color) : color =
			match c with
				| Black -> Red
				| Red -> Black
		;;

		let change_node_color (n : dict) : dict = 
			match n with
				(* Ensures leaves stay black *)
				| Leaf -> Leaf
				| Node(l, (k, v, c), r) -> Node(l, (k, v, change_color c), r) (* @Gabrielle: do we want to write this in terms of check_root_color? *)
		;;

		let rotate_left (d:dict) : dict  =
			match d with
				| Leaf -> raise(ArgumentError "Cannot right-rotate a leaf.")
				| Node(l, (k1, v1, c1), r) -> 
					match r with
						| Leaf -> raise(ArgumentError "Cannot left-rotate a node with a leaf as its right child.")
						| Node(rl, (k2, v2, c2), rr) -> Node(Node(l, (k1, v1, Red), rl), (k2, v2, c1), rr)
		;;

		let rotate_right (d:dict) : dict  =
			match d with
				| Leaf -> raise(ArgumentError "Cannot right-rotate a leaf.")
				| Node(l, (k1, v1, c1), r) -> 
					match l with
						| Leaf -> raise(ArgumentError "Cannot right-rotate a node with a leaf as its left child.")
						| Node(ll, (k2, v2, c2), lr) -> Node(ll, (k2, v2, c1), Node(lr, (k1, v1, Red), r))
		;;
		
		let color_flip (d : dict) : dict =
			match d with
				| Leaf -> raise(ArgumentError "Cannot color-flip a leaf.")
				| Node(l, (k, v, c), r) -> Node(change_node_color l, (k, v, change_color c), change_node_color r)
		;;
		
    let empty : dict =
			Leaf
    ;;			
		
		let insert_fix (d : dict) : dict =
			match d with
				| Leaf -> raise Impossible
				| Node (l, _, r) -> 
						match (check_root_color l, check_root_color r) with
							| (Black, Red) -> let d = rotate_left d in
									(match d with
										| Leaf -> raise Impossible
										| Node (l, _, r) -> 
												(match (check_root_color l, check_root_color r) with
													| (Red, Red) -> rotate_right d
													| _ -> d))
							| (Red, Red) -> rotate_right d
							| _ -> d		


    let rec insert (d:dict) (k:key) (v:value) : dict = 
      match d with
				| Leaf -> Node(Leaf, (k, v, Red), Leaf)
				| Node (l, _, r) ->
					match (check_root_color l, check_root_color r) with
						| (a, b) -> let d = (if (a = Red && b = Red) then color_flip d else d) in
								(match d with
									| Leaf -> raise Impossible
									| Node(l, (k2, v2, c), r) -> 
											(match D.compare k k2 with
												| Eq -> insert_fix d										
												| Less -> let d = Node(insert l k v, (k2, v2, c), r) in insert_fix d
												| Greater -> let d = Node(l, (k2, v2, c), insert r k v) in insert_fix d))
    ;;

    let rec lookup (d:dict) (k:key) : value option =
			match d with 
				| Leaf -> None
				| Node(l, (k2, v, _), r) -> 
					match D.compare k k2 with
						| Eq -> Some v
						| Less -> lookup l k
						| Greater -> lookup r k
    ;;
    
		let rec member (d:dict) (k:key) : bool = 
      match d with
				| Leaf -> false
				| Node(l, (k2, _, _), r) -> 
					match D.compare k k2 with
						| Eq -> true
						| Less -> member l k
						| Greater -> member r k
    ;;

    let remove (d:dict) (k:key) : dict = 
      raise ImplementMe
    ;;

    let rec choose (d:dict) : (key*value*dict) option = 
			match d with
				| Leaf -> None
				| Node(_, (k, v, _), _) -> Some (k, v, remove d k)
    ;;

    let fold (f:key -> value -> 'a -> 'a) (u:'a) (d:dict) : 'a = 
      raise ImplementMe
    ;;
  end
