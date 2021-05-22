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

let get_player_name (pl : t) : string = pl.name

(*generate bank cards*)
let rec gen_cards (card : Resource.t list) (num_needed : int) =
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
      @ gen_cards [ Wood ] 19
      @ gen_cards [ Brick ] 19
      @ gen_cards [ Wheat ] 19;
    dev_cards = [];
    points = 0;
  }

(** The type [tr] represents the type of a trade as a tuple of a player
    and the resource cards. *)
type tr = t * Resource.t list

(** [trade_out player_cards resources new_pl_res] returns a list of
    cards that removes the cards that a player wants to trade away.
    [player_cards] is the cards owned. [resources] is list of resources
    they're trading in. [new_pl_res] is an accumulator. *)
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
   from trading away the resources from the player, both specified in
   the trade [trade_tup], to be replaced with the resources in
   [gained_res].

   Raises [InvalidTrade] if one of the players is trades no cards in
   [trade_tup]. *)
let trade trade_tup gained_res with_bank =
  match trade_tup with
  | p, r_l ->
      if
        (* print_string ("player" ^ string_of_int p.num ^ " wants to
           trade away " ^ pp_list pp_resource r_l ^ "\n"); *)
        (* print_string ("player's cards are " ^ pp_list pp_resource
           p.cards ^ "\n"); *)
        with_bank || r_l <> []
      then
        (* print_string ("for trade out, cards are " ^ pp_list
           pp_resource p.cards ^ "\n"); *)
        let traded_out = trade_out p.cards r_l [] in
        (* print_string ("after replacing: " ^ pp_list pp_resource
           (traded_out @ gained_res) ^ "\n"); *)
        traded_out @ gained_res
      else raise InvalidTrade

(** [trade_to_player trade_1 trade_2 with_bank] returns a tuple of two
    players with newly traded cards. It removes resource cards from the
    player specified in [trade1] and adds them to the player in
    [trade2]. [with_bank] is a bool that is true if the first player is
    trading with the bank as the second player, and false otherwise.

    Note: [trade_1] must be the player of the current turn. This way,
    can be used to trade with bank, which must be [trade_2].

    Raises [InvalidTrade] if one of the players is trading no cards. *)
let trade_to_player trade_1 trade_2 with_bank =
  (*need to trade out-- remove the cards that currently has, then trade
    in-- add in cards they want*)
  match trade_1 with
  | p_1, res_1 -> (
      match trade_2 with
      | p_2, res_2 ->
          let player_1 =
            { p_1 with cards = trade trade_1 res_2 false }
          in
          let player_2 =
            if with_bank then
              { p_2 with cards = trade trade_2 res_1 true }
            else { p_2 with cards = trade trade_2 res_1 false }
          in
          (player_1, player_2))

let trade_to_bank player player_res bank_res =
  trade_to_player (player, player_res) (bank, bank_res) true

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
  (*todo: fix spaces*)
  let filtered_input = input |> String.split_on_char ' ' in
  matching_input filtered_input []

let trading_logic player1 player2 =
  print_string
    (" You currently have "
    ^ unmatch_input player1.cards ""
    ^ "What would you like to trade? \n ");
  print_string "> ";
  let trade1 = (player1, input_to_list (read_line ())) in
  print_string " What would you like to trade for? \n ";
  print_string "> ";
  let trade2 = (player2, input_to_list (read_line ())) in
  let player_1 = fst (trade_to_player trade1 trade2 false) in
  print_string ("Your cards now: " ^ unmatch_input player_1.cards "")

(*[dist_helper corners players] check if players have a building on any
  of those corners by checking and distribute accordingly by creating
  new players with those resources. [corners] is the corners of a tile.
  [players] is a player list. *)
let rec dist_helper corners players res =
  match corners with
  | [] -> players
  | h :: t -> (
      (*checks which player has that corner, generate a new list of
        players by replacing the player. [players_on_corner] should be a
        list of players that have new resources*)
      let node = corner_to_node h in
      match node with
      | Some settlement ->
          let new_player_list =
            update_pl_cards settlement.player_num players
              settlement.building res
          in
          dist_helper t new_player_list res
      | None -> dist_helper t players res)

(* [distr_res players_list rum] is the new players_list with distributed
   resources to all players in [players_list] based on the num rolled by
   the dice [num]*)
let distr_res (players_list : t list) (num : int) json : t list =
  (*Check which tiles have the same dice number (from tile list) *)
  let tiles = dice_roll_tiles num json in
  (*Find all the corners of those tiles *)
  let rec distr_per_tile tile_list new_pl_list =
    match tile_list with
    | [] ->
        List.rev new_pl_list
        (*Check if players have a building on any of those corners and
          distribute accordingly*)
    | h :: t ->
        distr_per_tile t
          (dist_helper h.corner_positions players_list h.resource)
  in
  distr_per_tile tiles []

(* [distr_res_setup players_list] is the new players_list with resources
   associated with all the homes built during the set up process*)
let distr_res_setup player house_loc json : player =
  let corner = house_loc in
  let tiles = tiles_from_json json in
  let tiles =
    List.filter (fun t -> List.mem corner t.corner_positions) tiles
  in
  let rec distr_tiles tile cards =
    match tile with
    | [] -> cards
    | h :: t -> distr_tiles t (h.resource :: cards)
  in
  let new_cards = distr_tiles tiles [] in
  { player with cards = new_cards }
