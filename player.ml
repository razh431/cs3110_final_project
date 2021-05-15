open Tile
open Resource
open Board
open Dev_cards

exception UnknownBuilding

exception InvalidTrade

let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let pp_resource = function
  | Wheat -> "Wheat"
  | Ore -> "Ore"
  | Wool -> "Wool"
  | Brick -> "Brick"
  | Wood -> "Wood"

(** The type [color] represents the colors of players. *)
type color =
  | Blue
  | Red
  | Green
  | Yellow
  | White
  | Magenta

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
  tiles : Tile.t list;
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

(** [gen_cards card num_needed] generates a given number [num_needed] of
    a kind of resource card [card]. Used to initialize the bank. *)
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

(** The type [tr] represents the type of a trade as a tuple of a player
    and the resource cards. *)
type tr = t * Resource.t list

(** [trade_out player_cards resources new_pl_res] returns a list of
    cards that removes the cards that a player wants to trade away.
    [player_cards] is the cards owned. [resources] is list of resources
    they're trading in. [new_pl_res] is an accumulator.*)
let rec trade_out
    (player_cards : Resource.t list)
    (resources : Resource.t list)
    (new_pl_res : Resource.t list) =
  (* print_string ("player cards before match " ^ pp_list pp_resource
     player_cards ^ "\n"); *)
  match player_cards with
  | [] ->
      (* print_string ("acc when [] " ^ pp_list pp_resource new_pl_res ^
         "\n"); *)
      (*player has no cards, but exists cards needed to be traded in*)
      (* if new_pl_res <> [] then new_pl_res else raise InvalidTrade *)
      new_pl_res
  | h :: t -> (
      match resources with
      | [] ->
          (* print_string ("acc1 " ^ pp_list pp_resource new_pl_res ^
             "\n"); *)
          new_pl_res @ player_cards
      | r :: ls ->
          (* print_string ("acc2 " ^ pp_list pp_resource new_pl_res ^
             "\n"); *)
          (*shorten list of res to trade in, check rest of cards*)
          if r = h then trade_out t ls new_pl_res
          else trade_out t resources (h :: new_pl_res))

(* [trade trade_tup gained_res] returns a list of resources resulting
   from trading away the resources from the player specified in the
   trade [trade_tup] to be replaced with the resources in [gained_res].

   Raises InvalidTrade if one of the players trades no cards. *)
let trade trade_tup (gained_res : Resource.t list) =
  match trade_tup with
  | p, r_l -> (
      (* print_string ("player" ^ string_of_int p.num ^ " wants to trade
         away " ^ pp_list pp_resource r_l ^ "\n"); *)
      (* print_string ("player's cards are " ^ pp_list pp_resource
         p.cards ^ "\n"); *)
      try
        (* print_string ("for trade out, cards are " ^ pp_list
           pp_resource p.cards ^ "\n"); *)
        let traded_out = trade_out p.cards r_l [] in
        (* print_string ("after replacing: " ^ pp_list pp_resource
           (traded_out @ gained_res) ^ "\n"); *)
        traded_out @ gained_res
      with InvalidTrade -> failwith "TODO: invalid trade")

(** [trade_to_player trade_1 trade_2] returns a tuple of two players
    with newly traded cards. It removes resource cards from the player
    specified in [trade1] and adds them to the player in trade2.

    Note: [trade_1] must be the player of the current turn. This way,
    can be used to trade with bank, which must be [trade_2] *)
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

let trade_to_bank player res_list =
  trade_to_player (player, res_list) (bank, [])

let player1 =
  {
    (init_player 1 "allison" Blue) with
    cards = [ Wool; Brick; Wool; Wood; Brick ];
  }

let player2 = { (init_player 1 "rachel" Red) with cards = [ Ore; Ore ] }
