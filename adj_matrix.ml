open Yojson.Basic.Util
open Resource
open Player
open Tile

type id = int

type dice_num = int

type resource = Resource.t

type resource_str = string

type corners = int list

type robber = bool

type tile = {
  id : id;
  dice_num : dice_num;
  resource : resource_str;
  corners : corners;
  robber : robber;
}

(* Type of a road in the adjacency matrix. [Some of int] describes a
   road owned by player with that number. If there is an unoccupied
   road, then the road is [Some 0]. If there is no connection then the
   road is [None].*)
type road =
  | Some of int
  | None

(* Type of a building, used in [corner], which describes the player who
   owns it and the kind of building it is. *)
type settlement = {
  player_num : int;
  building : Tile.building;
}

(* Type of a corner in the adjacency matrix. [Some of settlement]
   describes the player and kind of building if there is a building on
   that corner, otherwise it has type [None]. *)
type corner =
  | Some of settlement
  | None

(* type t = tile array array *)
type road_matrix = road array array

type corner_matrix = corner list

(* let resource_of_str str = *)

let tiles_from_json json =
  {
    id = json |> member "id" |> to_string |> int_of_string;
    dice_num = json |> member "dice num" |> to_string |> int_of_string;
    resource = json |> member "resource" |> to_string;
    corners = json |> member "corners" |> to_list |> filter_int;
    robber = json |> member "robber" |> to_string |> bool_of_string;
  }

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
