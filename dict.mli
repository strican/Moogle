module type DICT =
  sig
    type key
    type value
    type dict
    val empty : dict
    val insert : dict -> key -> value -> dict
    val lookup : dict -> key -> value option
    val remove : dict -> key -> dict
    val member : dict -> key -> bool
    val choose : dict -> (key * value * dict) option
    val fold : (key -> value -> 'a -> 'a) -> 'a -> dict -> 'a
  end
module type DICT_ARG =
  sig type key type value val compare : key -> key -> Order.order end
module AssocListDict :
  functor (D : DICT_ARG) ->
    sig
      type key = D.key
      type value = D.value
      type dict
      val empty : dict
      val insert : dict -> key -> value -> dict
      val lookup : dict -> key -> value option
      val remove : dict -> key -> dict
      val member : dict -> key -> bool
      val choose : dict -> (key * value * dict) option
      val fold : (key -> value -> 'a -> 'a) -> 'a -> dict -> 'a
    end
module RBTreeDict :
  functor (D : DICT_ARG) ->
    sig
      type key = D.key
      type value = D.value
      type dict
      val empty : dict
      val insert : dict -> key -> value -> dict
      val lookup : dict -> key -> value option
      val remove : dict -> key -> dict
      val member : dict -> key -> bool
      val choose : dict -> (key * value * dict) option
      val fold : (key -> value -> 'a -> 'a) -> 'a -> dict -> 'a
    end
