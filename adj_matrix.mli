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

(** [curr_roads] is the mutable adjacency matrix of roads. *)
val curr_roads : road array array

(** [update_road_mtx r c v] updates the roads matrix at row [r] and
    column [c] with value [v]. *)
val update_road_mtx : int -> int -> road -> unit

(*corners functions*)

(** [init_corners] makes the initial array of corners. *)
val init_corners : node array

(** [curr_corners] is the mutable array of corners (i.e. nodes). *)
val curr_corners : node array

(** [update_corners i v] updates the corner array at tile position [i]
    with node v*)
val update_corners : int -> node -> unit

(** [dice_roll_tiles num] returns a list of all the tiles with the dice
    roll number that corresponds to [num]. *)
val dice_roll_tiles : int -> Yojson.Basic.t -> tile list

(** [corners_rolled] finds all the corners of those tiles，check if
    players have a building on any of those corners, and update player's
    resources accordingly*)
val corners_rolled : int -> Yojson.Basic.t -> unit
