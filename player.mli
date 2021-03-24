type t

(* TODO: exceptions with 'of' for more info *)
exception UnknownBuilding

exception InvalidTrade

(** [trade_to_bank player building_name] updates the resources of the
    player according to the cost of the building specified by
    [building_name]. Raises [UnknownBuilding] for an invalid building.
    Raises [InvalidTrade] if the resources are not sufficient. name *)
val trade_to_bank : t -> Board.trade_in -> t

(** [trade_to_player (p1,p2)] returns a new tuple of players after a
    trade has been completed. Trade resources [Resource.t list] for
    resource [Resource.t list]. Raises [InvalidTrade] if one or more
    players has insufficient resources for the desired trade. *)
val trade_to_player :
  t * t -> Resource.t list -> Resource.t list -> t * t
