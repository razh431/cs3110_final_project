type t

<<<<<<< HEAD
(** The type of a trade. An association list where the key is the player
    and the value is the list of resources they want to trade away. *)
type tr = (t * Resource.t list) list
=======
>>>>>>> text_based_interface

(* TODO: exceptions with 'of' for more info *)
exception UnknownBuilding

exception InvalidTrade

type color =
  | Blue
  | Red
  | Green
  | Orange
  | White
  | Brown

  
val init_player : int -> string -> color -> t

(** [trade_to_bank player building_name] updates the resources of the
    player according to the cost of the building specified by
    [building_name]. Raises [UnknownBuilding] for an invalid building.
    Raises [InvalidTrade] if the resources are not sufficient. *)
val trade_to_bank : t -> Board.trade_in -> t

<<<<<<< HEAD
(** [trade_to_player tr] returns a new tuple of players whose lists of
    resources have been updated after a trade has been completed. Raises
    [InvalidTrade] if one or more players has insufficient resources for
    the desired trade.

    Ex. If p1 wants to trade 1 wool and 2 brick for p2's 2 lumber:
    trade_to_player
    [(p1,\[Wool, Brick, Brick\]); (p2, \[Lumber, Lumber\])] *)
val trade_to_player : tr -> t * t
=======
(** [trade_to_player (p1,p2)] returns a new tuple of players after a
    trade has been completed. Trade resources [Resource.t list] for
    resource [Resource.t list]. Raises [InvalidTrade] if one or more
    players has insufficient resources for the desired trade. *)
(* val trade_to_player :*)
  t * t -> Resource.t list -> Resource.t list -> t * t 
>>>>>>> text_based_interface
