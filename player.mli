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

type player = {
  name : string;
  num : int;
  color : color;
  cards : Resource.t list;
  dev_cards : Dev_cards.t list;
  tiles : Tile.t list;
  points : int;
}

type t = player

val init_player : int -> string -> color -> t

(** The type of a trade. a tuple of list and the list to trade out *)
type tr = t * Resource.t list

(** [trade_to_bank player building_name] updates the resources of the
    player according to the cost of the building specified by
    [building_name]. Raises [UnknownBuilding] for an invalid building.
    Raises [InvalidTrade] if the resources are not sufficient. *)

val trade_to_bank : t -> Resource.t list -> t * t

(** [trade_to_player tr] returns a new tuple of players whose lists of
    resources have been updated after a trade has been completed. Raises
    [InvalidTrade] if one or more players has insufficient resources for
    the desired trade. Ex. If p1 wants to trade 1 wool and 2 brick for
    p2's 2 lumber: trade_to_player (p1,\[Wool, Brick, Brick\]) (p2,
    \[Lumber, Lumber\]) *)
val trade_to_player : tr -> tr -> t * t

(* val bank : t *)

val player1 : t

val player2 : t
