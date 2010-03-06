val partner1 : string
val partner2 : string
val link_compare :
  Util.CrawlerServices.link -> Util.CrawlerServices.link -> Order.order
module LinkSet :
  sig
    type elt = Util.CrawlerServices.link
    type set
    val empty : set
    val is_empty : set -> bool
    val insert : elt -> set -> set
    val singleton : elt -> set
    val union : set -> set -> set
    val intersect : set -> set -> set
    val remove : elt -> set -> set
    val member : set -> elt -> bool
    val choose : set -> (elt * set) option
    val fold : (elt -> 'a -> 'a) -> 'a -> set -> 'a
  end
module WordDict :
  sig
    type key = string
    type value = LinkSet.set
    type dict
    val empty : dict
    val insert : dict -> key -> value -> dict
    val lookup : dict -> key -> value option
    val remove : dict -> key -> dict
    val member : dict -> key -> bool
    val choose : dict -> (key * value * dict) option
    val fold : (key -> value -> 'a -> 'a) -> 'a -> dict -> 'a
  end
module VisitSet :
  sig
    type elt = Util.CrawlerServices.link
    type set
    val empty : set
    val is_empty : set -> bool
    val insert : elt -> set -> set
    val singleton : elt -> set
    val union : set -> set -> set
    val intersect : set -> set -> set
    val remove : elt -> set -> set
    val member : set -> elt -> bool
    val choose : set -> (elt * set) option
    val fold : (elt -> 'a -> 'a) -> 'a -> set -> 'a
  end
module Q :
  sig
    type query = Word of string | And of query * query | Or of query * query
    val parse_words : string list -> query
    val query_re : Str.regexp
    val case_re : Str.regexp
    val term_sep_re : Str.regexp
    val remove_case_type : string list -> string list
    val print_list : string list -> unit
    val query_to_string : string -> string list
    val parse_query : string -> query * bool
    val eval_query : WordDict.dict -> query -> LinkSet.set
  end
module LinkQ :
  sig
    type 'a queue = 'a Queue_module.DoubleListQueue.queue
    val empty : unit -> 'a queue
    val enqueue : 'a -> 'a queue -> 'a queue
    val is_empty : 'a queue -> bool
    exception EmptyQueue
    val dequeue : 'a queue -> 'a queue
    val front : 'a queue -> 'a
    val insert_list : 'a list -> 'a queue -> 'a queue
  end
val modify_link_set :
  LinkSet.elt -> WordDict.dict -> WordDict.key -> WordDict.dict
val visit :
  VisitSet.elt ->
  VisitSet.set * VisitSet.elt LinkQ.queue ->
  VisitSet.set * VisitSet.elt LinkQ.queue
val bfs_loop :
  VisitSet.elt LinkQ.queue ->
  VisitSet.set -> WordDict.dict -> int -> bool -> WordDict.dict
val crawler : bool -> WordDict.dict
val std_response_header : string
val moogle_home_page : string
val input_lines : in_channel -> string list -> string list
val std_response : string
val query_response_header : string
val html_of_urlset : LinkSet.set -> string
val query_response_footer : string
val send_std_response : Unix.file_descr -> int
val http_get_re : Str.regexp
val hamming_loop : string -> string -> int -> int -> int
val hamming_d : string -> string -> int
val get_possible : WordDict.dict -> string -> WordDict.key list
val spell_suggest : WordDict.dict -> WordDict.key list -> WordDict.key list
val suggest_body : string list -> string
val process_request :
  Unix.file_descr -> string -> WordDict.dict * WordDict.dict -> int
val server : WordDict.dict * WordDict.dict -> 'a
val main : unit -> 'a
