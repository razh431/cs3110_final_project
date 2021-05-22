(* TODO: exceptions with 'of' for more info *)
exception UnknownBuilding

exception InvalidTrade

type color =
  | Blue
  | Red
  | Green
  | Yellow
  | White
  | Magenta

type player = {
  name : string;
  num : int;
  color : color;
  cards : Resource.t list;
  dev_cards : Dev_cards.t list;
  points : int;
}

type t = player

val init_player : int -> string -> color -> t

(** The type of a trade. a tuple of list and the list to trade out *)
type tr = t * Resource.t list

(** [update_player player cards dev_cards points] makes a new player
    with those fields*)
val update_player : t -> Resource.t list -> Dev_cards.t list -> int -> t

(** [gen_cards card num_needed] just creates a list of resource of
    number [num_needed]*)
val gen_cards : Resource.t list -> int -> Resource.t list

(** [trade_to_bank player player_res bank_res] updates the resources of
    the player according to the resources they wish to trade away in
    [player_res]. The resources that the bank is giving to the player is
    denoted by [bank_res]. *)
val trade_to_bank : t -> Resource.t list -> Resource.t list -> t * t

(** [trade_to_player tr1 tr2 with_bank] returns a new tuple of players
    whose lists of resources have been updated after a trade has been
    completed.

    Raises [InvalidTrade] if one or more players has insufficient
    resources for the desired trade. Also raises [InvalidTrade] if a
    trade between players involves one player trading no cards.

    Ex. If p1 wants to trade 1 wool and 2 brick for p2's 2 lumber:
    trade_to_player (p1,\[Wool, Brick, Brick\]) (p2, \[Lumber, Lumber\]) *)
val trade_to_player : tr -> tr -> bool -> t * t

(** [update_pl_cards pl_num pl_list building res] updates the cards of
    player with number [pl_num] from a list of players [pl_list].
    Returns an updated list of players. If [building] is [House], then
    they get one of [res]. If it is [City], they get two [res] cards.

    [pl_list] stores the players in decreasing order, i.e. [p3;p2;p1]*)
val update_pl_cards :
  int -> t list -> Adj_matrix.building -> Resource.t -> t list

(** [update_pl_settlements pl_num building loc] returns the updated
    corner array in Adj_matrix by adding a settlement [building] at the
    corner with id [loc].

    Raises [Adj_matrix.InvalidTileId i] if i is not in the range [1,54]. *)
val update_pl_settlements :
  int -> Adj_matrix.building -> int -> Adj_matrix.node array

(** [update_pl_roads pl_num v1 v2] returns the updated road matrix in
    Adj_matrix so that there is a road owned by player with number
    [pl_num] between points [v1] and [v2].

    Raises [Adj_matrix.InvalidRoad (v1,v2)] if at least one is not in
    the range [1,54]. *)
val update_pl_roads : int -> int -> int -> Adj_matrix.road array array

(** [update_pl_points pl_num pl_list] updates the points of the player
    with number [pl_num], from a list of players [pl_list] and returns
    an updated list of players. *)
val update_pl_points : int -> t list -> t list

val bank : t

(*input string into list of resource*)
val input_to_list : string -> Resource.t list

(*[trading logic p1 p2] trades cards between player 1 and player 2 and
  returns a tuple of p1 and p2*)
val trading_logic : t -> t -> t * t

(* [distr_res players_list num json ] is the new players_list with
   distributed resources to all players in [players_list] based on the
   num rolled by the dice [num]. Returns of list of players with updated
   cards in the same order*)
val distr_res : t list -> int -> Yojson.Basic.t -> t list

(* [distr_res_setup players_list json] is the new players_list with
   resources associated with all the homes built during the set up
   process*)
val distr_res_setup : t -> int -> Yojson.Basic.t -> player

val unmatch_input : Resource.t list -> string -> string
