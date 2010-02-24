module type SET =
  sig
    type elt
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
module type COMPARABLE = sig type t val compare : t -> t -> Order.order end
module ListSet :
  functor (C : COMPARABLE) ->
    sig
      type elt = C.t
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
