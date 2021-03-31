open Resource

(** The type representing settlements. *)
type building =
  | House
  | City

type building_opt =
  | None
  | Some of building

type edge = int * int

exception PlayerHasNoRoads

exception TileHasNoRobber of int

exception InvalidTile of int

(** The type [tile] represents a hex on the map.

    A tile has a resource [resource_name], a number corresponding to the
    tile for picking up resources when a dice is rolled [tile_dice_num],
    and a fixed position on the board [tile_position]. There are
    adjacent tiles [neighbor_tiles], corners with buildings [corners],
    and it can have the robber [robber]. [corners] is an association
    list where the 6 corners of the hex are mapped to a building.
    [roads] is an association list where the key is the number of the
    player who owns the road and the value is a list of edge tuples. If
    nobody owns the road, then the player number of that road is 9.

    Requires: [tile_position] is from 1-19.

    Ex. Edges of the first tile are (1,5), (5,9), (9,13), (13,8), (8,4),
    (4,1). If player 1 has a road on (1,5), player 2 has a road on
    (5,9), and nobody else has a road on the tile, then [roads] is
    [
      (0, \[(9,13);(8,13);(4,8);(1,4)\]); 
      (1, \[(1,5)\]); 
      (2, \[(5,9)\])
    ] *)

type tile = {
  resource_name : Resource.t;
  tile_dice_num : int;
  tile_position : int;
  neighbor_tiles : tile list;
  corners : (int * building_opt) list;
  robber : bool;
  roads : (int * edge list) list;
}

type t = tile

let resource hex = hex.resource_name

let dice_num hex = hex.tile_dice_num

let position hex = hex.tile_position

let roads_of_player hex player_num =
  try List.assoc player_num hex.roads
  with Not_found -> raise PlayerHasNoRoads

let has_robber hex = hex.robber

let move_robber (hex1, hex2) =
  let p1 = position hex1 in
  if p1 = position hex2 then raise (InvalidTile p1)
  else if has_robber hex1 = false then raise (TileHasNoRobber p1)
  else
    let hex3 = { hex1 with robber = false } in
    let hex4 = { hex2 with robber = true } in
    (hex3, hex4)

(* TODO: better implementation? *)
(* TODO: remove function from .mli file? *)
let neighbors_from_pos pos =
  if pos = 1 || pos = 2 then [] else failwith "TODO"

(* TODO: better implementation? *)
(* TODO: remove function from .mli file? *)
let edges_from_pos pos =
  if pos = 1 then [ (9, 13); (8, 13); (4, 8); (1, 4); (1, 5); (5, 9) ]
  else if pos = 2 then
    [ (2, 6); (6, 10); (10, 14); (9, 14); (5, 9); (2, 5) ]
  else failwith "TODO"

let make_tile str dice pos =
  let resource =
    match str with
    | "wheat" -> Wheat
    | "ore" -> Ore
    | "wool" -> Wool
    | "brick" -> Brick
    | "wood" -> Wood
    | _ -> raise (Invalid_argument str)
  in
  {
    resource_name = resource;
    tile_dice_num = dice;
    tile_position = pos;
    neighbor_tiles = neighbors_from_pos pos;
    corners =
      [
        (1, None); (2, None); (3, None); (4, None); (5, None); (6, None);
      ];
    robber = false;
    roads = [ (0, edges_from_pos pos) ];
  }
