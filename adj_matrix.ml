open Yojson.Basic.Util
open Resource
open Player

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
  corner_position : int list;
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

let resource_from_string input =
  match input with
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

(*make a single tile*)
let tile_from_json json =
  {
    id = json |> member "id" |> to_string |> int_of_string;
    dice_num = json |> member "dice num" |> to_string |> int_of_string;
    resource =
      json |> member "resource" |> to_string |> resource_from_string;
    corner_position = json |> member "corners" |> to_list |> filter_int;
    robber = json |> member "robber" |> to_string |> bool_of_string;
  }

let tiles_from_json json =
  json |> member "tiles" |> to_list |> List.map tile_from_json

  let curr_roads = init_road_mtx

let update_road_mtx
    (row : int)
    (column : int)
    (value : road) =
  curr_roads.(row).(column) <- value

let curr_corners = init_corners

let update_corners index (c : node) =
  curr_corners.(index) <- c

(*if we roll dice, we want a certain number, and get all the tiles ids*)
let dice_roll_tiles num json =
  List.filter (fun x -> num = x.dice_num) (tiles_from_json json)

(*check if players have a building on any of those corners, and update
  player's resources accordingly*)
let settlement_on_corner (x : node) =
  match x with
  | Some settlement -> (
      match settlement.building with
      (*failwith "todo: distrubute resources for player with house"*)
      | House -> true
      (*failwith "todo: distrubute resources for player wtih city"*)
      | City -> false)
  | None -> failwith "there are no settlements at this corner"

(*look through corner list for each corner, distrubute resources. *)
let corner_pos_to_node corner_pos = 
  let curr_corner = curr_corners in 



let corners_rolled num json =
  let rec update_pl_resource lst acc =
    match lst with
    | [] -> acc
    | h :: t -> 
      h.corner_position

  in

  update_pl_resource (dice_roll_tiles num json) []
