val monopoly : Resource.t -> Player.player -> Player.t

val victory_points : Player.t -> Player.t

val year_of_plenty : Player.t -> Resource.t list -> Player.t * Player.t

(* val knight : *)
val road_building : Player.player -> unit

val parse_road_str : string -> int list
