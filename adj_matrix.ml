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

<<<<<<< HEAD
(*if we roll dice, we want a certain number, and get all the tiles ids*)
=======
>>>>>>> 9d8e009c8978fa8da6a4ad39741683f6fdfc5c39
let dice_roll_tiles num json =
  List.filter (fun x -> num = x.dice_num) (tiles_from_json json)

(*********** New stuff?? START ************)

(** [settlement_on_corner res node] updates the player with the resource
    [res] associated with the corner [node]. *)
let settlement_on_node res = function
  | Some s -> (
      match s.building with
      | House -> Player.update_pl_cards s.player_num House res
      | City -> Player.update_pl_cards s.player_num City res)
  | None -> ()

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
  (* match tile with | [] -> failwith "TODO" | hd :: tl -> let c =
     curr_corners.(hd - 1) in settlement_on_node res c;
     distr_res_of_tile tl res *)
  List.map
    (fun node -> settlement_on_node tile.resource node)
    (pos_to_nodes tile.corner_positions [])

(*********** New stuff?? END ************)

(** [res_of_node corner_pos] returns the resource(s) of the tiles that
    care adjacent to the corner at position [corner_pos]. *)
let res_of_node corner_pos = failwith "TODO"

(** [distribute_res player_num res building] distributes resources to a
    player who has a building [building] on a node with resource [res]. *)
let distribute_res player_num res = function
  | House -> failwith "TODO"
  | City -> failwith "TODO"

(** [settlement_on_corner node] updates the player with the resource
    associated with the corner [node]. *)
let settlement_on_corner (x : node) =
  match x with
  | Some settlement -> (
      match settlement.building with
      (*failwith "todo: distrubute resources for player with house"*)
      | House -> true
      (*failwith "todo: distrubute resources for player wtih city"*)
      | City -> false)
  | None -> failwith "there are no settlements at this corner"

<<<<<<< HEAD
(*look through corner list for each corner, distrubute resources. *)
let corner_pos_to_node corner_pos = 
  let curr_corner = curr_corners in 


=======
(** For a given (numerical) corner position, update the resources of the
    player with a settlement there. *)
let corner_pos_to_node corner_pos =
  settlement_on_corner curr_corners.(corner_pos)
>>>>>>> 9d8e009c8978fa8da6a4ad39741683f6fdfc5c39

let corners_rolled num json =
  (* let rec update_pl_resource lst acc = match lst with [] -> acc | h
     :: t -> h.corner_position in

     update_pl_resource (dice_roll_tiles num json) [] *)
  ()
