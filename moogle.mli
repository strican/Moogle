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
module Q :
  sig
    type query = Word of string | And of query * query | Or of query * query
    val parse_words : string list -> query
    val query_re : Str.regexp
    val term_sep_re : Str.regexp
    val parse_query : string -> query
    val eval_query : WordDict.dict -> query -> LinkSet.set
  end
val crawler : unit -> WordDict.dict
val std_response_header : string
val moogle_home_page : string
val input_lines : in_channel -> string list -> string list
val std_response : string
val query_response_header : string
val html_of_urlset : LinkSet.set -> string
val query_response_footer : string
val send_std_response : Unix.file_descr -> int
val http_get_re : Str.regexp
val process_request : Unix.file_descr -> string -> WordDict.dict -> int
val server : WordDict.dict -> 'a
val main : unit -> 'a
