type t

(* TODO: exceptions with 'of' for more info *)
exception UnknownBuilding

exception InvalidTrade

(** [trade_to_bank player building_name] updates the resources of the
    player according to the cost of the building specified by
    [building_name]. Raises [UnknownBuilding] for an invalid building.
    Raises [InvalidTrade] if the resources are not sufficient. *)
val trade_to_bank : t -> Board.trade_in -> t

(** [trade_to_player tr] returns a new tuple of players whose lists of
    resources have been updated after a trade has been completed. Raises
    [InvalidTrade] if one or more players has insufficient resources for
    the desired trade. *)
val trade_to_player : t -> t -> Resource.t -> Resource.t -> t * t
