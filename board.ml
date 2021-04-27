open Random
open Resource
open Tile
open Dev_cards

(* TODO: unimplemented *)
type port = None

(** The type representing a Catan board.

    It has the tiles on the map [tiles], undrawn development cards
    [dev_cards], and ports [ports]. *)
type board = {
  tiles : Tile.t list;
  dev_cards : Dev_cards.t list;
  ports : port list;
}

type trade_in = {
  dev_cards : Dev_cards.t list;
  building : Adj_matrix.building;
}

type t = board
