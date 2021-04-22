type id = int

type dice_num = int

type resource = Resource.t

type robber = bool

(** The type of a building that players can build on a tile. *)
type building =
  | House
  | City

(* Type of a building, used in [corner], which describes the player who
   owns it and the kind of building it is. *)
type settlement = {
  player_num : int;
  building : building;
}

(* Type of a corner in the adjacency matrix. [Some of settlement]
   describes the player and kind of building if there is a building on
   that corner, otherwise it has type [None]. *)
type corner =
  | Some of settlement
  | None

(* Type of a road in the adjacency matrix. [Some of int] describes a
   road owned by player with that number. If there is an unoccupied
   road, then the road is [Some 0]. If there is no connection then the
   road is [None].*)
type road =
  | Some of int
  | None

type tile = {
  id : id;
  dice_num : dice_num;
  resource : resource;
  corners : corner list;
  robber : robber;
}

(* [tiles_from_json j] makes a list of tiles from the json*)
val tiles_from_json : Yojson.Basic.t -> tile list

(*[resource_from_string s] parses s into a Resource.t type*)
val resource_from_string : string -> Resource.t

(* [init_road_mtx] makes the inital road matrix that needs to be updated
   throughout the game*)
val init_road_mtx : road array array

(* [update_road_mtx r c v] updates the the matrix with value [v] into
   row [r] and column [c]*)
val update_road_mtx :
  road array array -> int -> int -> road -> road array array

(*corners functions*)

(* [init_corners] makes the initial corners list.*)
val init_corners : corner array

(* [update_corners a i v ] updates the corner array [a] at tile position
   [i] with corner v*)
val update_corners : corner array -> int -> corner -> corner array

(*[dice_roll_tiles num] if we roll dice, we want a certain number, and
  get all the tiles with that number in a list*)
val dice_roll_tiles : int -> tile list

(* [corners_rolled] finds all the corners of those tiles，check if
   players have a building on any of those corners, and update player's
   resources accordingly*)
val corners_rolled : unit
