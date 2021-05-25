(* Write parse stuffs *)

(* [check_name_str n] checks if the names are a correct format - Names
   cannot be the same when creating game; - Name cannot be empty string
   when creating game. names must be strings *)
(* val check_name_str : string -> player list -> string *)
(* - Numbers inputted for road building and home building cannot be
   greater than 54 or cannot be already created *)
(* - Cannot input an impossible road *)
(* - Check if valid player name when trying to trade Check valid
   resource name when trying to trade Check valid resource card name
   when trying to trade *)
(* When roll is typed, allow for phrases such as “please roll” or “yes
   roll” *)
exception Empty_Name

exception Repeated_Name

exception Letters_Name

exception RoadLength

exception InvalidRoadFormat

val is_alpha : char -> bool

val name_player : string -> Player.player list -> string

(** [parse_road_str str] parses a string of brackets, integers, and
    commas denoting a road into a list of integers. Raises
    [InvalidRoadFormat] if it cannot be parsed. *)
val parse_road_str : string -> int list

(** [check_road_input player str json] returns the string [str] if it is
    valid for player [player] to build the road specified in [str],
    which is a string of the form ["\[v1,v2\]"], containing two indices
    that represent a road. [json] represents valid roads.

    Raises [RoadLength] if the input string does not have two indices.
    Raises [Adj_matrix.InvalidRoad (v1,v2)] if at least one index is out
    of range [1,54]. Raises [Adj_matrix.OccupiedRoad (v1,v2)] if the
    road specified is already occupied. *)
val check_road_input : Player.t -> string -> Yojson.Basic.t -> string

(** [check_road_input index] returns corner index [i] if it is valid to
    build a house or city on that corner, i.e. it is in the bounds
    [1,54] and that index is not already occupied.

    Raises [Adj_matrix.OccupiedTileId idx] if corner [i] is already
    occupied. Raises [Adj_matrix.InvalidTileId i] if [i] is not in the
    bounds of [1,54]. *)
val check_corner_input : int -> int

val parse_help : string -> unit
