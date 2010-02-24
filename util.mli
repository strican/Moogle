module type CRAWLER_SERVICES =
  sig
    type link = { host : string; port : int; path : string; }
    val string_of_link : link -> string
    type page = { url : link; links : link list; words : string list; }
    val get_page : link -> page
    val initial_link : link
    val num_pages_to_search : int
    val server_port : int
  end
module CrawlerServices : CRAWLER_SERVICES
