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
  name : string;
  num : int;
  color : color;
  cards : Resource.t list;
  dev_cards : dev_card list;
  tiles : Tile.t list;
  points : int;
}

type t = player

(*match player's cards with the resources they need to trade in.
  [player_cards] is the cards owned. [new_pl_res] is an accumulator.
  [resources] is list of resources they're trading in. trade_out returns
  a list of cards, might be modified if successful trade, might not be*)
let rec trade_out player_cards resources new_pl_res =
  match player_cards with
  | [] ->
      (*player has no cards, but exists cards needed to be traded in*)
      print_endline
        "Invalid Trade. You don't have the needed resources.\n";
      player_cards
  | h :: t -> (
      match resources with
      | [] -> new_pl_res @ player_cards
      | r :: ls ->
          (*shorten list of res to trade in, check rest of cards*)
          if r = h then trade_out t ls new_pl_res
          else trade_out t resources (h :: new_pl_res))

let trade_to_bank player res_1 = failwith "unimplemented"
(*if the player's filtred cards = his origional cards, then they dont
  have that card*)

let trade_to_player player1 player2 res_1 res_2 =
  failwith "unimplemented"
(*check to see if the player1 has a resource1 card to give*)

(* Longest road for each player: if 6 is attached to a or e, then add 1.
   if not, then add it to b.

   1____2____3____4____5____(6) *)
(* | *)
(* 6 *)
