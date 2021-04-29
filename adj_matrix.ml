open Yojson.Basic.Util
open Resource
(* open Player *)

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

let init_road_mtx : road array array = Array.make_matrix 54 54 None

type settlement = {
  player_num : int;
  building : building;
}

type node =
  | Some of settlement
  | None

let init_corners = Array.make 54 None

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
    id = json |> member "id" |> to_string |> int_of_string;
    dice_num = json |> member "dice num" |> to_string |> int_of_string;
    resource =
      json |> member "resource" |> to_string |> resource_from_string;
    corner_positions = json |> member "corners" |> to_list |> filter_int;
    robber = json |> member "robber" |> to_string |> bool_of_string;
  }

let tiles_from_json json =
  json |> member "tiles" |> to_list |> List.map tile_from_json

let curr_roads = init_road_mtx

let update_road_mtx (row : int) (column : int) (value : road) =
  curr_roads.(row - 1).(column - 1) <- value

let curr_corners = init_corners

let update_corners index (c : node) = curr_corners.(index) <- c

let dice_roll_tiles num json =
  List.filter (fun x -> num = x.dice_num) (tiles_from_json json)
