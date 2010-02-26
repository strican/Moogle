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

		let change_color (c : color) : color =
			match c with
				| Black -> Red
				| Red -> Black
		;;

		let change_node_color (n : dict) : dict = 
			match n with
				| Leaf -> raise(ArgumentError "Cannot color-flip a leaf.")
				| Node(l, (k, v, c), r) -> Node(l, (k, v, change_color c), r)
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

    let get_color (d:dict) : color option =
      match d with
        | Leaf -> None
        | Node (l, (k, v, c), r) -> Some c
      ;;
      
    let fix_up (d:dict) : dict =
      match d with
        | Node((ll, (kl, vl, cl), rl),a,r) ->
          let d = (if(get_color r = Some Red) then rotate_left d else d) in
          (match d with
            | Node((ll, (kl, vl, cl), rl),a,r) ->
              let d = (if(cl = Red && get_color ll = Some Red) then rotate_right d else d) in
              (match d with
                | Node((ll, (kl, vl, cl), rl),a,r) ->
                  if (cl = Red && get_color r = Some Red) then color_flip d else d  
                | _ -> d)
            | _ -> d)
        | _ -> d
		;;

    let move_red_right (d:dict) : dict =
      d = color_flip d in 
      match d with
        | Node((ll, (kl, vl, cl), rl),a,r) -> let d = (if get_color ll = Some Red then 
          rotate_right d else d) in
          let d = color_flip d in
          d
        | _  -> d
     ;;

    let move_red_left (d:dict) : dict =
      d = color_flip d in 
      match d with
        | Node(l,a,r) -> let (ll, (kl, vl, cl), rl) = r in
        if get_color rl = Some Red then 
          let r = rotate_right r in
          let d = Node(l,a,r) (* Do I need this? *) in
          let d = rotate_left d in
          let d = color_flip d in
          d
        | _  -> d
     ;;

    let rec delete_min (d:dict) : dict =
      match d with
        | Node(Leaf,a,r) -> Leaf
        | Node((ll, (kl, vl, cl), rl),a,r) -> let d = (if cl != Red && 
           get_color ll != Some Red then move_red_left d else d) in
          (match d with
            | Node (l,a,r) -> l = delete_min l
            | _ -> fix_up d  ) in
           let d = Node(l,a,r) in fix_up d
         | _  -> fix_up d
    ;;

    let rec delete_max (d:dict) : dict =
      match d with
        | Node((ll, (kl, vl, cl), rl),a,r) -> let d = (if cl = Red 
          then rotate_right d else d) in
          (match d with
            | Node (l,a,Leaf) -> Leaf
            | Node(l,a,(rl, (kr, vr, cr), rr)) -> let d = (if cr != Red && 
                  get_color rr != Some Red then move_red_right d else d) in
                  (match d with
                    | Node (l,a,r) -> let r = deleteMax r in fix_up Node (l,a,r) 
                    | _ -> fix_up d
                    ) 
            | _ -> fix_up d  )
         | _  -> fix_up d
    ;;

    let remove (d:dict) (ke:key) : dict = 
      match d with
        | Node ((ll,(kl,vl,cl),rl), (k,v,c), r) -> 
        (match D.compare ke k  with
							| Less -> let d = (if cl != Red && get_color ll != Some Red 
                then move_red_left d else d )in
                (match d with 
                  | Node (l,a,r) -> let l = delete(l,ke) in fix_up Node (l,a,r)
                  | _ -> fix_up d)
              | _ -> (match d with
                | Node (l,a,r) -> let d = (if get_color l = Some Red then 
                rotate_right d else d) in
                (match d with 
                  | Node (l,(k,v,c),r) -> if D.compare k ke = Eq && r = Leaf then 
                     Leaf
                    (*else if get_color r != Some Red && let r = (rl,(kr,vr,cr), rr) in get_color rl != Some Red
                    then d = move_red_right d in
                    (match d with 
                      | Node (l,(k,v,c),r) -> if D.compare k ke = Eq then 
							
         | _ -> fix_up d*))))
    ;;

    let choose (d:dict) : (key*value*dict) option = 
      raise ImplementMe
    ;;

    let fold (f:key -> value -> 'a -> 'a) (u:'a) (d:dict) : 'a = 
      raise ImplementMe
    ;;
  end
