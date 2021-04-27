open Resource

(* open Board *)
open Dev_cards

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
  dev_cards : Dev_cards.t list;
  tiles : Adj_matrix.tile list;
  points : int;
}

type t = player

let init_player (number : int) (pl_name : string) (col : color) : t =
  {
    num = number;
    name = pl_name;
    color = col;
    cards = [];
    dev_cards = [];
    tiles = [];
    points = 0;
  }

let update_player player cards dev_cards tiles points =
  { player with cards; dev_cards; tiles; points }

let get_player_name (pl : t) : string = pl.name

(*generate bank cards*)
let rec gen_cards card num_needed =
  match card with
  | [] -> failwith "never called"
  | h :: t ->
      if num_needed = 1 then card
      else gen_cards (h :: card) (num_needed - 1)

let bank =
  {
    num = 0;
    name = "Bank";
    color = White;
    cards =
      gen_cards [ Wool ] 19
      @ gen_cards [ Ore ] 19
      @ gen_cards [ Wool ] 19
      @ gen_cards [ Brick ] 19;
    dev_cards = [];
    tiles = [];
    points = 0;
  }

type tr = t * Resource.t list

(** [trade_out] returns a list of cards that removes the cards they want
    to trade out. [player_cards] is the cards owned. [new_pl_res] is an
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

(*make new player with newly traded resources res*)
let trade trade_res (res : Resource.t list) =
  match trade_res with p, r_l -> trade_out p.cards r_l [] @ res

(** [trade_to_player trade_1 trade_2] creates two players with newly
    traded cards. It removes cards from the player from player1 and adds
    it to the player in trade2.

    Note: [trade_1] must be the player of the current turn. This way,
    can be used to trade with bank, which must be trade_2 *)
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

let trade_to_bank player player_res bank_res =
  trade_to_player (player, player_res) (bank, bank_res)

let update_pl_cards p_num building res = failwith "TODO"

(*check to see if the player1 has a resource1 card to give*)

(* Longest road for each player: if 6 is attached to a or e, then add 1.
   if not, then add it to b.

   1____2____3____4____5____(6) *)
(* | *)
(* 6 *)

(*see if this works*)

let player1 =
  {
    (init_player 1 "allison" Blue) with
    cards = [ Wool; Brick; Wool; Wood; Brick ];
  }

let player2 = { (init_player 1 "rachel" Red) with cards = [ Ore; Ore ] }
