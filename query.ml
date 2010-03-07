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
  let case_re = Str.regexp "case=sensitive"
  let term_sep_re = Str.regexp "\\+\\|&"
  
  let remove_case_type (words : string list) : string list =
    match List.rev words with
      | [] -> []
      | h::t -> List.rev t
  ;;
  
  (* Testing function *)
  let rec print_list (l : string list) =
    match l with
      | [] -> ()
      | h::t -> Printf.printf "%s\n" h ; flush_all (); print_list t
  ;;
  
  let query_to_string (s : string) : string list =
    if Str.string_match query_re s 0 then
      let qs = Str.matched_group 1 s in 
      let words = Str.split term_sep_re qs in
      let sens = (try Str.search_forward case_re s 0 with Not_found -> 0) in
      let case = Str.string_match case_re s sens in
        (if case then remove_case_type words 
         else List.map String.lowercase words)
    else raise (Failure "query not understood")
  ;;

  let parse_query s = 
    let q = query_to_string s in
    let sens = (try Str.search_forward case_re s 0 with Not_found -> 0) in
    let case = Str.string_match case_re s sens in
    (parse_words q, case)
  ;;

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
