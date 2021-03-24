open Random
open Resource
open Tile

(* TODO: unimplemented *)
type dev_card = None

(* TODO: unimplemented *)
type port = None

(** The type representing a Catan board.

    It has the tiles on the map [tiles], undrawn development cards
    [dev_cards], and ports [ports]. *)
type board = {
  tiles : Tile.t list;
  dev_cards : dev_card list;
  ports : port list;
}

type trade_in = {
  dev_cards : dev_card list;
  building : Tile.building;
}

type t = board
