type t

(** [build_house b pos] is the board [b] resulting from building a house
    on the tile at position [pos]. Requires: [pos] is from 1–19.
    Raises [OccupiedCorner] if the corner already has a road or
    settlement. *)
val build_house : Board.t -> int -> Board.t

(** [build_road b pos] is the board [b] resulting from building a road
    on the tile at position [pos]. Requires: [pos] is from 1–19. *)
val build_road : Board.t -> int -> Board.t

(* val trade : Board.t -> *)
