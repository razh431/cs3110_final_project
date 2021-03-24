open Tile
open Resource
open Board

exception UnknownBuilding

exception InvalidTrade

(** The type [color] represents the colors of players. *)
type color =
  | Blue
  | Red
  | Green
  | Orange
  | White
  | Brown

(** The type [player] represents a player.

    A player has a number [num], color represented by [color], a hand of
    resource cards [cards], a hand of development cards [dev_cards],
    tiles with settlements [tiles], and victory points [points]. *)
type player = {
  num : int;
  color : color;
  cards : Resource.t list;
  dev_cards : dev_card list;
  tiles : Tile.t list;
  points : int;
}

type t = player

let trade_to_bank = failwith "unimplemented"

let trade_to_player = failwith "unimplemented"

(* Longest road for each player: if 6 is attached to a or e, then add 1.
   if not, then add it to b.

   1____2____3____4____5____(6) *)
(*      |  *)
(*      6 *)
