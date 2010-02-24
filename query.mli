module type QUERY_ARG =
  sig
    module S :
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
    module D :
      sig
        type key = string
        type value = S.set
        type dict
        val empty : dict
        val insert : dict -> key -> value -> dict
        val lookup : dict -> key -> value option
        val remove : dict -> key -> dict
        val member : dict -> key -> bool
        val choose : dict -> (key * value * dict) option
        val fold : (key -> value -> 'a -> 'a) -> 'a -> dict -> 'a
      end
  end
module Query :
  functor (A : QUERY_ARG) ->
    sig
      type query =
          Word of string
        | And of query * query
        | Or of query * query
      val parse_words : string list -> query
      val query_re : Str.regexp
      val term_sep_re : Str.regexp
      val parse_query : string -> query
      val eval_query : A.D.dict -> query -> A.D.value
    end
