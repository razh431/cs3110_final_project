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
  curr_roads.(row).(column) <- value

let curr_corners = init_corners

let update_corners index (c : node) = curr_corners.(index) <- c

let dice_roll_tiles num json =
  List.filter (fun x -> num = x.dice_num) (tiles_from_json json)

(** [pl_num_to_pl num player_list] gets the player id and returns the
    player in a player list of 1 player. player_list should never be
    empty. *)
let pl_num_to_pl num player_list =
  List.hd (List.filter (fun x -> num = x.num) player_list)

(*update_pls_cards *)
let update_pl_cards p_num res_list =
  let player = pl_num_to_pl p_num [] in
  trade_to_bank player [] res_list

(** [settlement_on_corner res node] updates the player with the resource
    [res] associated with the corner [node]. *)
let settlement_on_node res = function
  | Some s -> (
      match s.building with
      | House -> update_pl_cards s.player_num [ res ]
      | City -> update_pl_cards s.player_num [ res; res ])
  | None -> update_pl_cards 0 []

(** [pos_to_nodes positions acc] converts a list of corner positions
    into a list of the corresponding nodes. *)
let rec pos_to_nodes positions acc =
  match positions with
  | [] -> acc
  | hd :: tl -> pos_to_nodes tl (curr_corners.(hd - 1) :: acc)

(** [distr_res_of_tile tile] distributes the resource on the tile [tile]
    to all the players with settlements on the adjacent tiles (who share
    the corner positions). *)
let rec distr_res_of_tile tile =
  List.map
    (fun node -> settlement_on_node tile.resource node)
    (pos_to_nodes tile.corner_positions [])

let dice_roll_logic num json =
  List.map (fun x -> distr_res_of_tile x) (dice_roll_tiles num json)
