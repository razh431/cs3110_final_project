exception InvalidRoadId of int * int

exception OccupiedRoad of int * int

exception RoadNotConnected of int * int

exception InvalidTileId of int

exception OccupiedTileId of int

type id = int

type dice_num = int

type resource = Resource.t

type robber = bool

(** The type of a building that players can build on a tile. *)
type building =
  | House
  | City

(** Type of a settlement located at a node; describes the player who
    owns it and the kind of building it is. *)
type settlement = {
  player_num : int;
  building : building;
}

(** Type of a corner in the adjacency matrix. [Some settlement]
    describes the player and kind of building, if there is a building on
    that corner; otherwise it has type [None]. *)
type node =
  | Some of settlement
  | None

(** Type of a road in the adjacency matrix. [Some n] describes a road
    owned by player with number [n]. If there is an unoccupied road,
    then the road is [Some 0]. If there is no connection then the road
    is [None].*)
type road =
  | Some of int
  | None

type tile = {
  id : id;
  dice_num : dice_num;
  resource : resource;
  corner_positions : int list;
  robber : robber;
}

(** [tiles_from_json j] makes a list of tiles from the json*)
val tiles_from_json : Yojson.Basic.t -> tile list

(** [resource_from_string s] parses s into a Resource.t type*)
val resource_from_string : string -> Resource.t

(** [init_road_mtx] makes the inital road matrix. *)
val init_road_mtx : road array array

(*[curr_roads] is the current road matrix*)

val curr_roads : road array array

(** [update_road_mtx r c v] returns the updated road matrix with value
    [v] into row [r] and column [c] and row [c] and column [c]. r and c
    range from [1, 54].

    Raises [InvalidRoadId (r,c)] if at least one is out of bounds.
    Raises [OccupiedRoad (r,c)] if the road between [r] and [c] is
    already occupied. *)
val update_road_mtx : int -> int -> road -> road array array

(*corners functions*)

(** [init_corners] makes the initial array of corners.*)
val init_corners : node array

(** [curr_corners] is the mutable array of corners (i.e. nodes). *)
val curr_corners : node array

(** [update_corners i v] updates the corner array at tile position [i]
    on the board with node [v]. Corner 1, as labeled on the board, has
    index 0 in the array.

    Raises [InvalidTileId i] if the tile position is not in the range
    [1,54]. Raises [OccupiedTileId i] if the tile at [i] is already
    occupied. *)
val update_corners : int -> node -> node array

(** [curr_corners] is the current corners list*)
val curr_corners : node array

(** [dice_roll_tiles num] if we roll dice, we want a certain number, and
    get all the tiles with that number in a list*)
val dice_roll_tiles : int -> Yojson.Basic.t -> tile list

(** [corner_to_node num] returns the node corresponding to the corner,
    which is the int*)
val corner_to_node : int -> node
