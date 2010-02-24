(* The main moogle crawler and server *)

let partner1 = "strican";;
let partner2 = "gehrlich";;

open Util ;;    
open CrawlerServices ;;
open Order ;;

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
module LinkSet = Set.ListSet(struct 
                               type t = link
                               let compare = link_compare
                             end) ;;

(* Dictionaries mapping words (strings) to sets of crawler links *)
(* You should replace this invocation of the WordDict functor with
 * your RBTreeDict functor. *)
module WordDict = Dict.AssocListDict(struct 
                                       type key = string
                                       type value = LinkSet.set
                                       let compare = string_compare
                                     end) ;;

(* A query module that uses LinkSet and WordDict *)
module Q = Query.Query(struct
                         module S = LinkSet
                         module D = WordDict
                       end
                       ) ;;


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
let crawler () = 
   (* change this definition -- this just constructs a dummy 
    * dictionary. *)
  WordDict.insert WordDict.empty "moogle" (LinkSet.singleton initial_link) 
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
 * query to the query parser to get query abstract syntax.  Then we evaluate the
 * query, using the index we built earlier, to get a set of links.  Then we put
 * the result in an html document to send back to the client.  If we don't
 * understand the request, then we send the default page (which is just
 * moogle.html in this directory).  *)
let process_request client_fd request index = 
  try 
    let _ = Str.search_forward http_get_re request 0 in
    let query_string = Str.matched_group 1 request in
    let query = Q.parse_query query_string in
    let links = Q.eval_query index query in
    let response_body = html_of_urlset links in
    let response = 
      query_response_header ^ response_body ^ query_response_footer in
      Unix.send client_fd response 0 (String.length response) [] 
  with _ -> send_std_response client_fd
;; 

(* open a socket on the server port (specified on the command line), 
 * prepare it for listening, and then loop, accepting requests and
 * sending responses. 
 *)
let server (index:WordDict.dict) = 
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
  let index = crawler () in 
  let _ = Printf.printf "Index has been constructed; entering server loop.\n" in
  let _ = Printf.printf "Press Ctrl-c to terminate Moogle." in
  let _ = flush_all () in
    server index
;;

main() ;;

