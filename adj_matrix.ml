open Yojson.Basic.Util
open Resource
open Player

type id = int

type dice_num = int

type resource = Resource.t

type resource_str = string

type corners = int list

type robber = bool

type building =
  | House
  | City

type building_opt =
  | None
  | Some of building

type tile = {
  id : id;
  dice_num : dice_num;
  resource : resource;
  corners : corners;
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

type corner =
  | Some of settlement
  | None

type corner_matrix = corner list

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
    corners = json |> member "corners" |> to_list |> filter_int;
    robber = json |> member "robber" |> to_string |> bool_of_string;
  }

let tiles_from_json json =
  json |> member "tiles" |> to_list |> List.map tile_from_json

let update_road_mtx
    (matrix : road array array)
    (row : int)
    (column : int)
    (value : road) : road array array =
  matrix.(row).(column) <- value;
  matrix

let init_corners = Array.make 54 None

let update_corners arr index (c : corner) =
  arr.(index) <- c;
  arr

(*if we roll dice, we want a certain number, and get all the tiles ids*)
let dice_roll_tiles num json =
  List.filter (fun x -> num = x.dice_num) (tiles_from_json json)

let players_corners (x : corner) = 
  match x with 
| Some settlement -> 
  match settlement.building with 
    (*failwith "todo: distrubute resources for player with house"*)
  | House -> true
  (*failwith "todo: distrubute resources for player wtih city"*)
  | City -> false
| None -> failwith "there are no settlements at this corner"

let corners_rolled num json = 
  let tiles = dice_roll_tiles num json in 
  let rec update_pl_resource lst acc = 
    match tiles with 
    | [] -> acc
    | h :: t -> 
      List.filter (fun x -> players_corners x) h.corners