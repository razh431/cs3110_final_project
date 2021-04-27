(* * Representation of a Catan tile. *)

(** The abstract type of a hex. *)
type t = Adj_matrix.tile

(** The type of an edge. Must be specified with the point with a lower
    number first.

    Ex. (1,3) is a valid edge, but (3,1) is not. *)
type edge = int * int

exception PlayerHasNoRoads

exception TileHasNoRobber of int

exception InvalidTile of int

(** [resource hex] is the resource of tile [hex] *)
val resource : t -> Resource.t

(** [dice_num hex] is the dice-roll number on the tile [hex]. *)
val dice_num : t -> int

(** [position hex] is the indexed position of the tile [hex]. *)
val position : t -> int

(** [has_robber hex] returns whether or not the robber is on the tile
    [hex]. *)
val has_robber : t -> bool

(** [move_robber (hex1, hex2)] moves the tile from the tile with the
    robber [hex1] to the tile [hex2] and returns the two updated tiles.

    Requires: the two tiles are distinct. Raises [InvalidTile n] if the
    tiles are the same, where n is the position of the tile. Raises
    [TileHasNoRobber pos] if [hex1] does not have the robber, where
    [pos] is the numerical position of [hex1]. *)
val move_robber : t * t -> t * t

(** [edges_from_pos pos] returns the surrounding edges of a tile at
    position [pos]. *)
val edges_from_pos : int -> edge list
