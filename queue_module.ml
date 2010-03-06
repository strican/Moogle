(* Courtesy of Greg Morrisett *)
(* Lecture 5 - Slide 41 *)

module type QUEUE = 
  sig
    type 'a queue
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
    type 'a queue = {front: 'a list; rear: 'a list}
    let empty() = {front = []; rear = []}
    let enqueue x q = {front = q.front; rear = x::q.rear}
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