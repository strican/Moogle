module type QUERY_ARG = 
sig
  module S : Set.SET with type elt = Util.CrawlerServices.link
  module D : Dict.DICT with type key = string
                       with type value = S.set
end

module Query(A : QUERY_ARG) = 
struct
  open A ;;
             
  type query = 
      Word of string
    | And of query * query 
    | Or of query * query
        
  (* This is a hack -- we should probably do a proper job
   * of parsing with parentheses. *)
  let rec parse_words ws = 
    match ws with 
      | w::"AND"::rest -> And(Word w,parse_words rest)
      | w::"OR"::rest -> Or(Word w,parse_words rest)
      | w::[] -> Word w
      | w::rest -> And(Word w,parse_words rest)
      | [] -> raise (Failure "query not understood")
          
  let query_re = Str.regexp "\\?q=\\(.*\\)"
  let term_sep_re = Str.regexp "\\+"
    
  let parse_query s = 
    if Str.string_match query_re s 0 then 
      let qs = Str.matched_group 1 s in 
      let words = Str.split term_sep_re qs 
      in 
        parse_words words
    else raise (Failure "query not understood")
      
  (* Evaluate a query given an index *)
  let rec eval_query (idx : D.dict) (q:query) : S.set = 
    match q with 
      | Word x -> (match D.lookup idx x with 
                     | None -> S.empty
                     | Some s -> s)
      | And (q1,q2) -> 
          S.intersect (eval_query idx q1) (eval_query idx q2)
      | Or (q1,q2) -> 
          S.union (eval_query idx q1) (eval_query idx q2)
end
