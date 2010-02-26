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
    open Order ;;
    type key = D.key ;;
    type value = D.value ;;

		type color = Red | Black
    type dict = Leaf | Node of dict * (key * value * color) * dict ;;

    exception ImplementMe ;;
		exception ArgumentError of string ;;
		exception Impossible ;;

    (* Test cases *)
   (* let a = Leaf;;
    let b = Node(Leaf, (1, 2, Black), Leaf);;
    let c = Node(Node(Leaf, (2, 3, Red), Leaf), (4, 5, Black), Leaf);;
    let d = Node(Node(Leaf, (0, 1, Red), Leaf), (2, 3, Black), Node(Leaf, (7, 8, Red), Leaf));;
*)
    (* Retrieve arbitrary node in dict by following string path composed of l's and r's *)
		let rec get_node (d : dict) (s : string) : dict =
      if (s = "") then d else
			match d with
				| Node(l, _, r) -> 
					(match (String.sub s 0 1) with
						| "l" -> get_node l (String.sub s 1 (String.length s - 1))
						| "r" -> get_node r (String.sub s 1 (String.length s - 1))
						| _ -> raise (ArgumentError "Node doesn't exist"))
        | Leaf -> raise (ArgumentError "Node doesn't exist")
		;;
(*
    assert ((get_node a "") = Leaf);;
    assert ((get_node b "") = b);;
    assert ((get_node b "l") = Leaf);;
    assert ((get_node c "l") = Node(Leaf, (2, 3, Red), Leaf));;
*)
		let check_root_color (n : dict) : color = 
			match n with
				| Leaf -> Black
				| Node(_, (_, _, c), _) -> c 
		;;
  (*  
    assert ((check_root_color a) = Black);;
    assert ((check_root_color b) = Black);;
    assert ((check_root_color (get_node c "l")) = Red);;
*)
		let change_color (c : color) : color =
			match c with
				| Black -> Red
				| Red -> Black
		;;
(*
    assert ((change_color Black) = Red);;
    assert ((chnage_color Red) = Black);;
*)
		let change_node_color (n : dict) : dict = 
			match n with
				(* Ensures leaves stay black *)
				| Leaf -> Leaf
				| Node(l, (k, v, c), r) -> Node(l, (k, v, change_color c), r) (* @Gabrielle: do we want to write this in terms of check_root_color? *)
		;;
(*
    assert ((check_root_color (change_node_color a)) = Black);;
    assert ((check_root_color (change_node_color b)) = Red);;
    assert ((check_root_color (change_node color (get_node "l"))) = Black);;
*)
		let rotate_left (d:dict) : dict  =
			match d with
				| Leaf -> raise(ArgumentError "Cannot right-rotate a leaf.")
				| Node(l, (k1, v1, c1), r) -> 
					match r with
						| Leaf -> raise(ArgumentError "Cannot left-rotate a node with a leaf as its right child.")
						| Node(rl, (k2, v2, c2), rr) -> Node(Node(l, (k1, v1, Red), rl), (k2, v2, c1), rr)
		;;
(*
    assert ((rotate_left a) = ArgumentError);;
    assert ((rotate_left b) = ArgumentError);;
    assert ((rotate_left c) = ArgumentError);;
    assert ((rotate_left d) = Node(Node(Node(Leaf, (0, 1, Red), Leaf), (2, 3, Red), Leaf), (7, 8, Black), Leaf));;
*)
		let rotate_right (d:dict) : dict  =
			match d with
				| Leaf -> raise(ArgumentError "Cannot right-rotate a leaf.")
				| Node(l, (k1, v1, c1), r) -> 
					match l with
						| Leaf -> raise(ArgumentError "Cannot right-rotate a node with a leaf as its left child.")
						| Node(ll, (k2, v2, c2), lr) -> Node(ll, (k2, v2, c1), Node(lr, (k1, v1, Red), r))
		;;
		(*
    assert ((rotate_left a) = ArgumentError);;
    assert ((rotate_left b) = ArgumentError);;
    assert ((rotate_left c) = ArgumentError);;
    assert ((rotate_left d) = Node(Leaf, (0, 1, Black), Node(Leaf, (2, 3, Red), Node(Leaf, (7, 8, Red), Leaf))));;
    *)
		let color_flip (d : dict) : dict =
			match d with
				| Leaf -> raise(ArgumentError "Cannot color-flip a leaf.")
				| Node(l, (k, v, c), r) -> Node(change_node_color l, (k, v, change_color c), change_node_color r)
		;;
		(*
    assert ((color_flip a) = ArgumentError);;
    assert ((color_flip b) = Node(Leaf, (1, 2, Red), Leaf));;
    assert ((color_flip c) = Node(Node(Leaf, (2, 3, Black), Leaf), (4, 5, Red), Leaf));;
    assert ((color_flip d) = Node(Node(Leaf, (0, 1, Black), Leaf), (2, 3, Red), Node(Leaf, (7, 8, Black), Leaf)));;
    *)
    let empty : dict =
			Leaf
    ;;			
		(*
    assert ((empty) = Leaf);;
    *)
(*		let insert_fix (d : dict) : dict =
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
    ;;*)

    let rec insert (d:dict) (k:key) (v:value) : dict = 
      match d with
				| Leaf -> Node(Leaf, (k, v, Red), Leaf)
				| Node (l, (k2, _, c2), r) ->
          let d = 
          (match D.compare k k2 with
            | Eq -> Node(l, (k, v, c2), r)
            | Less -> insert l k v
            | Greater -> insert r k v) in
          let d = (if check_root_color (get_node d "r") = Red then rotate_left d else d) in
          let d = (if (check_root_color (get_node d "l") = Red && check_root_color (get_node d "ll") = Red) then rotate_right d else d) in
          d
        
    ;;
          
          
(*          
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
*)
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

(*  @ Gabrielle: I used check_root_color, which doesn't return an option, *)
(*               so i think it might be better. Also, leaves are always *)
(*               supposed to be black. I think CLRS said that
    let get_color (d:dict) : color option =
      match d with
        | Leaf -> None
        | Node (l, (k, v, c), r) -> Some c
      ;;
*)
      
    let fix_up (d:dict) : dict =
      match d with
        | Leaf -> Leaf
        | Node(l, _, r) -> 
          let d = (if (check_root_color r = Red) then rotate_left d else d) in
          let d = (if (check_root_color (get_node d "l") = Red && check_root_color (get_node d "ll") = Red) then rotate_right d else d) in
          let d = (if (check_root_color (get_node d "l") = Red && check_root_color (get_node d "r") = Red) then color_flip d else d) in
          d
    ;;
          
    let move_red_right (d:dict) : dict =
      let d = color_flip d in
      if (check_root_color (get_node d "ll") = Red) then color_flip (rotate_right d) else d
    ;;

    let move_red_left (d:dict) : dict =
      let d = color_flip d in 
      match d with
        | Leaf -> raise (ArgumentError "Cannot move red left on a leaf.")
        | Node(l, a, r) -> 
            if (check_root_color (get_node r "l") = Red) then color_flip (rotate_left (Node(l, a, rotate_right r))) else d
      ;;

    let rec delete_min (d:dict) : dict =  
      match d with
        | Leaf -> Leaf
        | Node(Leaf, _, _) -> Leaf
        | Node(l, a, r) -> 
          let d = (if (check_root_color l = Black && check_root_color (get_node l "l") = Black) then move_red_left d else d) in
          fix_up (Node(delete_min (get_node d "l"), a, r))
    ;;

(*
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
	*)	


		let get_key (d:dict) : key =
      match d with
        | Leaf -> raise (ArgumentError "Key DNE")
        | Node(_,(k,_,_),_) -> k
    ;;

    let rec min (d:dict) : key =
      let l = get_node d "l" in
      if (l = Leaf) then get_key d
      else min (l)
    ;;
    
    let extract (a : 'a option) : 'a =
      match a with
        | None -> raise (ArgumentError "Value doesn't exist.")
        | Some b -> b
    ;;
    
    
    let rec remove (d:dict) (k:key) : dict = 
      match d with
				| Leaf -> Leaf
				| Node(l, (k1, v1, c1), r) -> 
						match D.compare k k1 with									
								| Less -> 
										let d = (if (check_root_color l = Black && check_root_color (get_node d "ll") = Black) then move_red_left d else d) in fix_up (Node(remove (get_node d "l") k, (k1, v1, c1), get_node d "r"))
								| Greater | Eq -> let d = (if check_root_color l = Red then rotate_right d else d) in
																	let d = (if (D.compare k (get_key d)  = Eq && get_node d "r" = Leaf) then Leaf 
                                  
                                  else(
																	  let d = (if (check_root_color (get_node d "r") = Black && check_root_color (get_node d "rl") = Black) then move_red_right d else d) in
																	  
                                    let d = (if D.compare k (get_key d) = Eq then 
                                      (let r = get_node d "r" in 
                                       let v = lookup r (min r)  in
                                       let k2 = (min r) in 
                                       let r = delete_min r in 
                                       fix_up (Node(l, (k2, extract v, c1), r)))
                                      
                                      
                                      (*(match v with
                                         | None -> raise (ArgurmentError "Right DNE")
                                         | Some w -> let k2 = min r in let r = delete_min r) in fix_up Node (l,(k2,w,c1), r))*)
                                    else ((remove (get_node d "r") k))) in d)) in fix_up d
    ;;
                                  



(*


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
    ;;*)

    let rec choose (d:dict) : (key*value*dict) option = 
			match d with
				| Leaf -> None
				| Node(_, (k, v, _), _) -> Some (k, v, remove d k)
    ;;

    let rec fold (f:key -> value -> 'a -> 'a) (u:'a) (d:dict) : 'a = 
        match d with 
			    | Leaf -> u
			    | Node(l, (k, v, _), r) ->  f k v (fold f (fold f u r) l) 
    ;;
 
(*
    let print_tree (d : dict) : () = 
      fold (fun k v a -> print ()*)

  end
