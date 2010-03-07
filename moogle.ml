(* The main moogle crawler and server *)

let partner1 = "strican";;
let partner2 = "gehrlich";;

open Util ;;    
open CrawlerServices ;;
open Order ;;
open Queue_module ;;

let link_compare (x:link) (y:link) : order = 
  match string_compare x.host y.host with 
    | Eq -> 
        (match int_compare x.port y.port with 
           | Eq -> (match string_compare x.path y.path with 
                      | Eq -> Eq 
                      | ans -> ans)
           | ans -> ans)
    | ans -> ans
;;

(* Sets of crawler links *)
(* You should replace this invocation of the ListSet functor with 
 * your RBTreeSet functor. *)
module LinkSet = Set.DictSet(Dict.RBTreeDict(struct 
                               type key = link
															 type value = unit 
                               let compare = link_compare
                             end)) ;;

(* Dictionaries mapping words (strings) to sets of crawler links *)
(* You should replace this invocation of the WordDict functor with
 * your RBTreeDict functor. *)
module WordDict = Dict.RBTreeDict(struct 
                                       type key = string
                                       type value = LinkSet.set
                                       let compare = string_compare
                                     end) ;;
(*module LinkSet = Set.ListSet(struct 
                               type t = link
                               let compare = link_compare
                             end) ;;

module WordDict = Dict.AssocListDict(struct 
                                       type key = string
                                       type value = LinkSet.set
                                       let compare = string_compare
                                     end) ;;

module VisitSet = Set.ListSet(struct 
                               type t = link
                               let compare = link_compare
                             end) ;;*)

(* Set that tracks whether a link has been visited *)
module VisitSet = Set.DictSet(Dict.RBTreeDict(struct
                                        type key = link
                                        type value = unit
                                        let compare = link_compare
                                    end)) ;;

(* A query module that uses LinkSet and WordDict *)
module Q = Query.Query(struct
                         module S = LinkSet
                         module D = WordDict
                       end
                       ) ;;


module LinkQ = Queue_module.DoubleListQueue ;;


(********************************************************************)
(* TODO                                                             *)
(********************************************************************)
(* Implement a routine to create an index (i.e., WordDict) mapping
 * words to the URLs for the pages on which they appear.  You
 * should start with the link CrawlerServices.initial_link and use 
 * the CrawlerServices.get_page function to implement this.  Also,
 * you should only crawl CrawlerServices.num_pages_to_search pages
 * total.  
 *    
 *)

let modify_link_set (l : link) (d : WordDict.dict) (a : string) : 
      WordDict.dict =
    let new_set =
      (match WordDict.lookup d a with
         | None -> LinkSet.singleton l
         | Some set -> LinkSet.insert l set) in
    WordDict.insert d a new_set
;;

let visit (l : link) ((v, q): VisitSet.set * link LinkQ.queue) : 
      (VisitSet.set * link LinkQ.queue) =
  if ((VisitSet.member v l) = true) then (v, q)
  else (VisitSet.insert l v, LinkQ.enqueue l q)
;;

let rec bfs_loop (q : link LinkQ.queue) (v : VisitSet.set) 
                 (d : WordDict.dict) (ct : int) (case : bool) : 
                      WordDict.dict  =
  if ct >= CrawlerServices.num_pages_to_search then d
  else if LinkQ.is_empty q then d
	else
	  let l = LinkQ.front q in
    let _ = Printf.printf "%s\n" (CrawlerServices.string_of_link l) in
		let q = LinkQ.dequeue q in
		let p = CrawlerServices.get_page l in
		let wds = (if case then p.words else List.map String.lowercase p.words) in
    let (v, q) = List.fold_right visit p.links (v, q) in
			bfs_loop q v (List.fold_left (modify_link_set l) d wds) (ct + 1) case
;;


let crawler (case : bool) : WordDict.dict = 
  let q = LinkQ.empty() in
  let q = LinkQ.enqueue CrawlerServices.initial_link q in
  bfs_loop q (VisitSet.singleton CrawlerServices.initial_link) 
           WordDict.empty 0 case
;;


(*****************************************************************************)

let std_response_header = 
  "HTTP/1.1 200 OK\r\n" ^
  "Server: Moogle/0.0\n" ^
  "content-type: text/html; charset=utf-8\n" ^
  "Content-Language: en-us\n" ^
  "Connection: close\n\n"
;;

let moogle_home_page = "./moogle.html" ;; 
let moogle_search_bar = "<p>
<form>Search: <input type=\"text\" name=\"q\" />
	Case Sensitive: <input type=\"checkbox\" name=\"case\" value=\"sensitive\" />
</form>
</br>"

(* read in all the lines from a file and concatenate them into
 * a big string. *)
let rec input_lines inchan lines = 
  try 
    input_lines inchan ((input_line inchan)::lines)
  with End_of_file -> List.rev lines
;;

(* Build a message that has the default Moogle home page to send
 * to clients.  The contents of the home page can be found in 
 * the file moogle.html. *)
let std_response = 
  let ch = open_in moogle_home_page in 
  let lines = input_lines ch [] in 
  let resp = std_response_header ^ (String.concat "" lines) in
    close_in ch ; resp
;;

(* The header for search responses to clients. *)
let query_response_header = 
  std_response_header ^ 
  "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">" ^
    "<html> <head> <title>Moogle Search Results</title></head>" ^
    "<body><h1>Moogle Search Results:</h1><p><ul>"
;;

(* Convert a set of url's to HTML to splice into the search
 * response we send to clients. *)
let html_of_urlset : LinkSet.set -> string = 
  LinkSet.fold (fun link s -> "<li><a href=\"" ^ 
                  (string_of_link link) ^ "\">" ^ 
                  (string_of_link link) ^ "</a></li>" ^ s) ""
;;
          
(* The footer for search responses to clients. *)
let query_response_footer = "</ul><hr></body></html>"
;;

let send_std_response client_fd = 
  Unix.send client_fd std_response 0 (String.length std_response) []
;;

let http_get_re = 
  Str.regexp_case_fold "GET[ \t]+/\\([^ \t]*\\)[ \t]+HTTP/1\\.[0-9]"
;;

(* process a request -- we're expecting a GET followed by a query
 * "?q=word+word".  If we find something that this matches, then we feed the
 * query to the query parser to get query abstract syntax.  Then we evaluate 
 * the query, using the index we built earlier, to get a set of links.  Then
 * we put the result in an html document to send back to the client.  If we 
 * don't understand the request, then we send the default page (which is just
 * moogle.html in this directory).  *)
let rec hamming_loop (s1: string) (s2:string) (l : int) (i :int) : int =
  let x = if i >= l then 0 
  else (if (String.sub s1 i 1 = String.sub s2 i 1) then 0 else 1)
         + hamming_loop s1 s2 l (i+1) in
  x
  ;;


let hamming_d (s1 : string) (s2 : string) : int =
  try
    let _ = assert (String.length s1 == String.length s2) in
    hamming_loop s1 s2 (String.length s1) 0
    
  with _ -> 100
;;

let get_possible (ind : WordDict.dict) (a : string) : string list =
  WordDict.fold (fun k v acc -> (if (hamming_d a k) <= 1 then k::acc else acc))
     [] ind
;;

let spell_suggest (index : WordDict.dict) (words : string list) : string list =
  let misspell = List.filter (fun a -> not (WordDict.member index a)) words in
  List.fold_left (fun acc a -> (get_possible index a) @ acc) [] misspell
;;  

let suggest_body (wds : string list) : string = 
  moogle_search_bar ^ "<h4>Did you mean...</h4>" ^ 
    (List.fold_left (fun s wd -> "<li>" ^ wd ^ "</li>" ^ s) "" wds)
;;

let process_request client_fd request index = 
  try 
    let _ = Str.search_forward http_get_re request 0 in
    let query_string = Str.matched_group 1 request in
    let (query, case) = Q.parse_query query_string in
    let ind = if case then fst index else snd index in
    let links = Q.eval_query ind query in
    let response_body = if (LinkSet.is_empty links) 
        then suggest_body (spell_suggest ind (Q.query_to_string query_string))
        else html_of_urlset links in
    let response = 
      query_response_header ^ response_body ^ query_response_footer in
      Unix.send client_fd response 0 (String.length response) []
  with _ -> send_std_response client_fd
;; 

(* open a socket on the server port (specified on the command line), 
 * prepare it for listening, and then loop, accepting requests and
 * sending responses. 
 *)
let server (index : (WordDict.dict * WordDict.dict)) = 
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in 
  let sock_addr = Unix.ADDR_INET (Unix.inet_addr_any, server_port) in 
  let _ = Unix.bind fd sock_addr in
  let _ = Unix.listen fd 5 in  (* at most 5 queued requests *)
  let rec server_loop () = 
    (* allow a client to connect *)
    let (client_fd, client_addr) = Unix.accept fd in 
    let buf = String.create 4096 in
    let len = Unix.recv client_fd buf 0 (String.length buf) [] in
    let request = String.sub buf 0 len in
    (*let case = in*)
    (*let request=(if (case) then String.lowercase request else request) in*)
    let _ = process_request client_fd request index in
      Unix.close client_fd ; 
      server_loop() in 
    server_loop()
;;

(* On startup, create the index and then start the web server loop *)
let main () = 
  let _ = Printf.printf "Starting Moogle on port %d, indexing %d pages.\n" 
    server_port num_pages_to_search in
  let _ = flush_all () in
  let index = (crawler true, crawler false) in 
  let _ = Printf.printf "Index has been constructed; entering server loop.\n" in
  let _ = Printf.printf "Press Ctrl-c to terminate Moogle." in
  let _ = flush_all () in
    server index
;;

main() ;;