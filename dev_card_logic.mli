exception InvalidRoadFormat

val monopoly : Player.player -> Player.t

val victory_points : Player.t -> Player.t

val year_of_plenty : Player.t -> Player.t

(* val knight : *)
val road_building : Player.t -> Player.t

val parse_road_str : string -> int list

val dev_card_logic : Player.t -> Player.t
