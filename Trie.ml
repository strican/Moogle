(* UNSUCCESSFUL!!!!!!!!!!!!!!!!!!!!*)
(* Woulda been cool.......

module type TRIE = 
  sig
    type 'a trie
    val empty : unit -> 'a queue
    val enqueue : 'a -> 'a queue -> 'a queue
    val is_empty : 'a queue -> bool
    exception EmptyQueue
    val dequeue : 'a queue -> 'a queue
    val front : 'a queue -> 'a
    val insert_list : 'a list -> 'a queue -> 'a queue
  end

module DoubleListQueue : QUEUE =
  struct
    type 'a trie = {value: char option; children: char trie list}
    let empty() = {value = None; children = []}
    
    let single (s : string) : 'a trie =
      
    
    let insert_loop (s : string) (t : 'a trie) (c : int) =
      match t.value with
        | None -> single
      if (String.get s c) = t.value
    
    let insert s t = 
      if (String.get s 0) {front = q.front; rear = x::q.rear}
    let is_empty q =
      match q.front, q.rear with
        | [], [] -> true
        | _, _ -> false
    exception EmptyQueue
    let deq (q: 'a queue) : 'a * 'a queue = 
      match q.front with
        | h::t -> (h, {front = t; rear = q.rear})
        | [] -> match List.rev q.rear with
                  | h::t -> (h, {front = t; rear = []})
                  | [] -> raise EmptyQueue
    let dequeue (q: 'a queue) : 'a queue = snd(deq q)
    let front (q: 'a queue) : 'a = fst(deq q)
    let insert_list (l : 'a list) (q : 'a queue) : 'a queue =
      List.fold_left (fun b a -> enqueue a b) q l;;
  end
  
  
  
  (*reduce (fun a b -> if (get_page a).visted = true then b else (get_page a).visited = true a::b)*)
  
  
  *)