(** [monopoly pl] returns a new player that accepts all the cards of one
    resource from the other players*)
val monopoly : Player.player -> Player.t

(** [victory_points pl] returns a player with a point added *)
val victory_points : Player.t -> Player.t

(** [year_of_plenty pl] returns a new player that takes two resources of
    the player's choice*)
val year_of_plenty : Player.t -> Player.t

val road_building : Player.t -> Yojson.Basic.t -> Player.t

val dev_card_logic : Player.t -> Yojson.Basic.t -> Player.t

val dev_to_string : Dev_cards.t list -> string -> string
