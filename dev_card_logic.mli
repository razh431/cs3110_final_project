(** [monopoly player] returns a new player that accepts all the cards of
    one resource from the other players*)
val monopoly : Player.player -> Player.t

(** [victory_points player] returns a player with a point added *)
val victory_points : Player.t -> Player.t

(** [year_of_plenty player] returns a new player that takes two
    resources of the player's choice *)
val year_of_plenty : Player.t -> Player.t

(** [road_building player] returns a new player that prompts the user to
    build a road at the player's choice *)
val road_building : Player.t -> Yojson.Basic.t -> Player.t

(** [dev_card_logic player roads_json] parses the user's command to use
    a development card. *)
val dev_card_logic : Player.t -> Yojson.Basic.t -> Player.t

(** [dev_to_string dev_cards acc] converts [dev_cards] to a printable
    string *)
val dev_to_string : Dev_cards.t list -> string -> string
