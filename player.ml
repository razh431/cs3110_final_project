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

type dev_card =
  | Temp1
  | Temp2

(** The type [player] represents a player.

    A player has a number [num], color represented by [color], a hand of
    resource cards [cards], a hand of development cards [dev_cards],
    tiles with settlements [tiles], and victory points [points]. *)
type player = {
  name : string;
  num : int;
  name: string;
  color : color;
  cards : Resource.t list;
  dev_cards : dev_card list;
  tiles : Tile.t list;
  points : int;
}

<<<<<<< HEAD

type t = player

let init_player (number:int) (pl_name:string) (col:color) : t=
  {
    num= number;
    name = pl_name;
    color= col;
    cards= [];
    dev_cards= [];
    tiles= [];
    points= 0;
  }

let get_player_name (pl:t) : string = pl.name 
(* let trade_to_bank = failwith "unimplemented"*)

(* let trade_to_player = failwith "unimplemented" *)
=======
type t = player

let make_player =
  {
    name = "rachel";
    num = 1;
    color = Blue;
    cards = [ Wood; Wool; Wheat; Ore; Ore; Ore ];
    dev_cards = [];
    tiles = [];
    points = 2;
  }

let make_player_a =
  { make_player with cards = [ Brick; Wheat; Ore; Ore; Ore ] }

let make_player_1 =
  { make_player with name = "mindy"; cards = [ Brick; Wood; Ore ] }

let make_player_1a =
  { make_player_1 with cards = [ Wood; Wool; Ore; Wood ] }

type tr = t * Resource.t list

(*trade_out returns a list of cards that removes the cards they want to
  trade out. [player_cards] is the cards owned. [new_pl_res] is an
  accumulator. [resources] is list of resources they're trading in. *)
let rec trade_out
    (player_cards : Resource.t list)
    (resources : Resource.t list)
    (new_pl_res : Resource.t list) =
  match player_cards with
  | [] ->
      (*player has no cards, but exists cards needed to be traded in*)
      raise InvalidTrade
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

(*make new player with newly traded resources res*)
let trade trade_res (res : Resource.t list) =
  match trade_res with p, r_l -> trade_out p.cards r_l [] @ res

let trade_to_player trade_1 trade_2 =
  (*need to trade out-- remove the cards that currently has, then trade
    in-- add in cards they want*)
  match trade_1 with
  | p_1, res_1 -> (
      match trade_2 with
      | p_2, res_2 ->
          let player_1 = { p_1 with cards = trade trade_1 res_2 } in
          let player_2 = { p_2 with cards = trade trade_2 res_1 } in
          (player_1, player_2))

(*check to see if the player1 has a resource1 card to give*)
>>>>>>> player

(* Longest road for each player: if 6 is attached to a or e, then add 1.
   if not, then add it to b.

   1____2____3____4____5____(6) *)
(* | *)
(* 6 *)
