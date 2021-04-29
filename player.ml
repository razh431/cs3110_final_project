open Resource
open Board
open Dev_cards
open Adj_matrix

exception UnknownBuilding

exception InvalidTrade

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
    resource cards [cards], a hand of development cards [dev_cards], and
    victory points [points]. *)
type player = {
  name : string;
  num : int;
  color : color;
  cards : Resource.t list;
  dev_cards : Dev_cards.t list;
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
    points = 0;
  }

let update_player player cards dev_cards points =
  { player with cards; dev_cards; points }

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

(** [front_of_list pl_num lst] returns the elements of player list [lst]
    up to player with number [pl_num], not inclusive.

    E.g. [front_of_list 3 \[p4;p3;p2;p1\]] returns \[p4\]*)
let front_of_list pl_num lst =
  let rec front_aux pl_num lst acc =
    match lst with
    | [] -> List.rev acc
    | hd :: tl ->
        if pl_num = hd.num then List.rev acc
        else front_aux pl_num tl (hd :: acc)
  in
  front_aux pl_num lst []

(** [back_of_list pl_num lst] returns the elements of player list [lst]
    after player with number [pl_num], not inclusive.

    E.g. [back_of_list 3 \[p4;p3;p2;p1\]] returns \[p2;p1\] *)
let back_of_list pl_num lst =
  let rec back_aux pl_num lst acc =
    match lst with
    | [] -> acc
    | hd :: tl ->
        if pl_num = hd.num then acc else back_aux pl_num tl (hd :: acc)
  in
  back_aux pl_num (List.rev lst) []

(* Get the front and back of the list; update the cards of the player
   with number pl_num; reconstruct the list of players *)
let update_pl_cards pl_num pl_list building res =
  let front = front_of_list pl_num pl_list in
  let back = back_of_list pl_num pl_list in
  let player = List.nth pl_list pl_num in
  match building with
  | House -> front @ [ fst (trade_to_bank player [] [ res ]) ] @ back
  | City ->
      front @ [ fst (trade_to_bank player [] [ res; res ]) ] @ back

let update_pl_settlements pl_num building loc =
  Adj_matrix.update_corners loc (Some { player_num = pl_num; building })

let update_pl_roads (pl_num : int) v1 v2 =
  Adj_matrix.update_road_mtx v1 v2 (Some pl_num)

let update_pl_points pl_num pl_list = failwith "TODO"

let player1 =
  {
    (init_player 1 "allison" Blue) with
    cards = [ Wool; Brick; Wool; Wood; Brick ];
  }

let player2 = { (init_player 1 "rachel" Red) with cards = [ Ore; Ore ] }

let rec unmatch_input (res_list : Resource.t list) (acc : string) =
  match res_list with
  | [] -> acc
  | h :: t ->
      if h = Wool then unmatch_input t ("Wool " ^ acc)
      else if h = Ore then unmatch_input t ("Ore " ^ acc)
      else if h = Wood then unmatch_input t ("Wood " ^ acc)
      else if h = Brick then unmatch_input t ("Brick " ^ acc)
      else if h = Wheat then unmatch_input t ("Wheat " ^ acc)
      else failwith "incorrect command"

let rec matching_input
    (input_filtered : string list)
    (acc : Resource.t list) =
  match input_filtered with
  | [] | [ "" ] -> acc
  | h :: t ->
      matching_input t (Adj_matrix.resource_from_string h :: acc)

let input_to_list input =
  (*input string into list of string words*)
  (*todo: fix spaces*)
  let filtered_input = input |> String.split_on_char ' ' in
  matching_input filtered_input []

let trading_logic =
  print_string
    (" You currently have "
    ^ unmatch_input player1.cards ""
    ^ "What would you like to trade? \n ");
  print_string "> ";
  let trade1 = (player1, input_to_list (read_line ())) in
  print_string " What would you like to trade for? \n ";
  print_string "> ";
  let trade2 = (player2, input_to_list (read_line ())) in
  let player_1 = fst (trade_to_player trade1 trade2) in
  print_string ("Your cards now: " ^ unmatch_input player_1.cards "")
