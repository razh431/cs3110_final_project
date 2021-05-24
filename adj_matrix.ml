open Yojson.Basic.Util
open Resource

(* open Player *)

exception InvalidRoadId of int * int

exception OccupiedRoad of int * int

exception RoadNotConnected of int * int

exception InvalidTileId of int

exception OccupiedTileId of int

type id = int

type dice_num = int

type resource = Resource.t

type resource_str = string

type robber = bool

type building =
  | House
  | City

type tile = {
  id : id;
  dice_num : dice_num;
  resource : resource;
  corner_positions : int list;
  robber : robber;
}

type road =
  | Some of int
  | None

let init_road_mtx : road array array = Array.make_matrix 55 55 None

type settlement = {
  player_num : int;
  building : building;
}

type node =
  | Some of settlement
  | None

let init_corners = Array.make 55 None

let resource_from_string = function
  | "Wool" -> Wool
  | "wool" -> Wool
  | "Ore" -> Ore
  | "ore" -> Ore
  | "Wood" -> Wood
  | "wood" -> Wood
  | "Brick" -> Brick
  | "brick" -> Brick
  | "Wheat" -> Wheat
  | "wheat" -> Wheat
  | _ -> failwith "resource can't be parsed"

(** [tile_from_json] extracts a single tile from a json [json]. *)
let tile_from_json json =
  {
    id = json |> member "id" |> to_int;
    dice_num = json |> member "dice num" |> to_int;
    resource =
      json |> member "resource" |> to_string |> resource_from_string;
    corner_positions = json |> member "corners" |> to_list |> filter_int;
    robber = json |> member "robber" |> to_bool;
  }

let tiles_from_json json =
  json |> member "tiles" |> to_list |> List.map tile_from_json

(** [road_from_json json] extracts a single road from a json [json] as a
    tuple of integers. *)
let road_from_json json =
  let lst = json |> member "vertices" |> to_list in
  match lst with
  | [ x; y ] ->
      let v1 = x |> to_int in
      let v2 = y |> to_int in
      (v1, v2)
  | _ -> failwith "JSON file error: road is formed by two vertices"

(** [roads_from_json json] extracts the roads of the json as a list of
    tuples representing valid roads, e.g. [(1,4); (1;5); ...]*)
let roads_from_json json =
  json |> member "roads" |> to_list |> List.map road_from_json

let curr_roads = init_road_mtx

(** Raises [InvalidRoadId (row, column)] if [row] or [column] are out of
    bounds. Both must be in the range [1,54], inclusive, and be a valid
    road contained in [json]. Raises [OccupiedRoad (row, column)] if the
    road between [row] and [column] is already occupied. *)
let update_road_mtx row column (value : road) json =
  let valid_roads = roads_from_json json in
  if not (List.mem (row, column) valid_roads) then
    raise (InvalidRoadId (row, column))
  else if
    curr_roads.(row).(column) <> None
    && curr_roads.(column).(row) <> None
  then raise (OccupiedRoad (row, column))
  else curr_roads.(row).(column) <- value;
  curr_roads.(column).(row) <- value;
  curr_roads

let curr_corners = init_corners

(** Raises [InvalidTileId index] if [index] is out of the range [1,54],
    inclusive. Raises [OccupiedTileId index] if [index] is already
    occupied. *)
let update_corners index (c : node) =
  if index < 1 || index > 54 then raise (InvalidTileId index)
  else if curr_corners.(index) <> None then raise (OccupiedTileId index)
  else curr_corners.(index) <- c;
  curr_corners

let dice_roll_tiles num json =
  List.filter (fun x -> num = x.dice_num) (tiles_from_json json)

let corner_to_node num = curr_corners.(num)
