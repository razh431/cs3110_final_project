open Resource

(** The type representing settlements. *)
type building =
  | House
  | City

type building_opt =
  | None
  | Some of building

(** The type [tile] represents a hex on the map.

    A tile has a resource [resource_name], a number corresponding to the
    tile for picking up resources when a dice is rolled [tile_dice_num],
    a fixed position on the board [tile_position], adjacent tiles
    [neighbor_tiles], corners with buildings [corners], and it can have
    the robber [robber].

    Requires: [tile_position] is from 1-19 *)

type tile = {
  resource_name : Resource.t;
  tile_dice_num : int;
  tile_position : int;
  neighbor_tiles : tile list;
  corners : building_opt list;
  robber : bool;
}

type t = tile
