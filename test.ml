open OUnit2
open Player
open Resource

(********************************************************************
  Our approach to testing our Catan project

  Player module: Test cases were developed with black box and glass box
  testing. We believe this is the best way because some functions used
  pattern matching, so to ensure correctness, we tested cases where
  resources being traded away were at different and extremal locations
  in the list. Glass box testing was used to test different traversals
  of our control flow.

  Adj_matrix module: Test cases were developed using black box and glass
  box testing. Glass box testing was used to check that the different
  errors, raised in different if statements, were correctly raised. The
  constants for testing the matrix and the array are long because if the
  mutable array or adjacency matrix is not inside of each output
  constant, the output would be incorrect.

  Main module: ???????
  ********************************************************************)

(** [cmp_lists lst1 lst2] compares two set-like lists. *)
let cmp_lists lst1 lst2 =
  let sorted1 = List.sort compare lst1 in
  let sorted2 = List.sort compare lst2 in
  sorted1 = sorted2

(** [cmp_tup_lists tup1 tup2] compares two tuples of resource lists to
    see if the tuples are equivalent lists. The order of tuple matters,
    but order of the list elements does not matter. Each of [tup1] and
    [tup2] is a tuple of resource lists, representing the cards of each
    player after a trade.

    Ex. [tup1] = (\[Ore; Wool; Wool; Brick\], \[Wood;Ore\]) and [tup2] =
    (\[Wool; Wool; Brick; Ore\], \[Ore;Wood\]) *)
let cmp_tup_of_lists tup1 tup2 =
  let tup1_fst = List.sort compare (fst tup1) in
  let tup1_snd = List.sort compare (snd tup1) in
  let tup2_fst = List.sort compare (fst tup2) in
  let tup2_snd = List.sort compare (snd tup2) in
  tup1_fst = tup2_fst && tup1_snd = tup2_snd

let move_robber_tests = [ assert true ]

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
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

(** [pp_tup_list pp_elt lst] pretty-prints a tuple of two lists [tup],
    using [pp_elt] to pretty-print each element of [tup]. *)
let pp_tup_list pp_elt tup =
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
  "([" ^ pp_elts (fst tup) ^ "], [" ^ pp_elts (snd tup) ^ "])"

let pp_string s = "\"" ^ s ^ "\""

(** [pp_resource r] pretty-prints resource [r]. *)
let pp_resource = function
  | Wheat -> "Wheat"
  | Ore -> "Ore"
  | Wool -> "Wool"
  | Brick -> "Brick"
  | Wood -> "Wood"

(** [pp_array pp_elt arr] pretty-prints array [arr], using [pp_elt] to
    pretty-print each element of [arr]. *)
let pp_array pp_elt arr =
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
  let l = Array.to_list arr in
  "[" ^ pp_elts l ^ "]"

(** [pp_building b] pretty-prints building [b]. *)
let pp_building = function
  | Adj_matrix.House -> "H"
  | Adj_matrix.City -> "C"

(** [pp_pnum num] pretty-prints a player number [num]. *)
let pp_pnum = function
  | 1 -> "player 1"
  | 2 -> "player 2"
  | 3 -> "player 3"
  | 4 -> "player 4"
  | _ -> failwith "only 4 players in a game"

(** [pp_node node] pretty-prints a node [node]. *)
let pp_node (n : Adj_matrix.node) =
  match n with
  | None -> "None"
  | Some (s : Adj_matrix.settlement) -> (
      match s with
      | { player_num = num; building = b } ->
          pp_pnum num ^ " has " ^ pp_building b)

(********************************************************************
  Start helper functions for testing.
  ********************************************************************)

(** [cards_from_trade trade_result num] returns the cards of one of the
    players in the tuple [trade_result], resulting from a trade. [num]
    denotes which of the players' cards we want, and it must be 1 or 2.

    Ex. A trade between players p1 and p2 results in (p1,p2). Then
    [cards_from_trade (p1, p2) 2] returns [p2.cards] *)
let cards_from_trade trade_result = function
  | 1 -> (fst trade_result).cards
  | 2 -> (snd trade_result).cards
  | _ -> failwith "There are only two players in a trade"

(** Test trade of resource cards in between two players. *)
let trade_pl_test name trade1 trade2 expected_output =
  let trade_result = trade_to_player trade1 trade2 false in
  let p1_cards = cards_from_trade trade_result 1 in
  let p2_cards = cards_from_trade trade_result 2 in
  let trade_result_cards = (p1_cards, p2_cards) in
  name >:: fun _ ->
  assert_equal expected_output ~cmp:cmp_tup_of_lists
    ~printer:(pp_tup_list pp_resource)
    trade_result_cards

(** Tests when [InvalidTrade] is raised in trades between players. *)
let trade_pl_err_test name trade1 trade2 expected_output =
  name >:: fun _ ->
  assert_raises expected_output (fun () ->
      trade_to_player trade1 trade2 false)

(** Tests when [InvalidTrade] is raised in trades between player and the
    bank. *)
let trade_bank_err_test name trade1 bank_res expected_output =
  name >:: fun _ ->
  match trade1 with
  | player, player_res ->
      assert_raises expected_output (fun () ->
          trade_to_bank player player_res bank_res)

(** [trade_bank_test name trade1 bank_res expected_output] tests a
    player trading resources into the bank and receiving [bank_res] in
    return. [bank_res] may be empty. *)
let trade_bank_test name trade1 bank_res expected_output =
  match trade1 with
  | pl, res ->
      let trade_result = trade_to_bank pl res bank_res in
      let p1_cards = cards_from_trade trade_result 1 in
      let bank_output_cards = cards_from_trade trade_result 2 in
      let trade_result_cards = (p1_cards, bank_output_cards) in
      name >:: fun _ ->
      assert_equal expected_output ~cmp:cmp_tup_of_lists
        ~printer:(pp_tup_list pp_resource)
        trade_result_cards

(** [update_roads_test name num v1 v2 expected_output] tests that the
    road matrix is updated at [v1][v2] and [v2][v1] with [Some num].
    [v1] and [v2] are valid indices in the range [1,54]. *)
let update_roads_test name num v1 v2 expected_output =
  name >:: fun _ ->
  assert_equal expected_output (update_pl_roads num v1 v2)

(** [update_roads_err_test name num v1 v2 expected_output] tests that
    updating road matrix with out of bounds [v1] and [v2] raises
    [Adj_matrix.InvalidRoad (v1,v2)]. *)
let update_roads_err_test name num v1 v2 expected_output =
  name >:: fun _ ->
  assert_raises expected_output (fun () -> update_pl_roads num v1 v2)

(** [update_corners_test name num building index expected_output] tests
    that the corner matrix is updated at [index] and [Some corner],
    where corner is a record containing player number [num] and building
    [building]. [index] is a valid index in the range [1,54]. *)
let update_corners_test name num building index expected_output =
  name >:: fun _ ->
  assert_equal expected_output ~printer:(pp_array pp_node)
    (update_pl_settlements num building index)

(** [update_corners_err_test name num building index expected_output]
    tests that updating the corner matrix with out of bounds [i] raises
    [Adj_matrix.InvalidTileId i]. *)
let update_corners_err_test name num building index expected_output =
  name >:: fun _ ->
  assert_raises expected_output (fun () ->
      update_pl_settlements num building index)

(********************************************************************
  Start constants for testing.
  ********************************************************************)
let p1 =
  { (init_player 1 "a" Blue) with cards = [ Wool; Wool; Brick; Wood ] }

let p2 = { (init_player 2 "b" Red) with cards = [ Ore; Ore ] }

let p3 =
  {
    (init_player 3 "c" Green) with
    cards =
      [ Wheat; Wheat; Ore; Ore; Wool; Wool; Brick; Brick; Wood; Wood ];
  }

(* trading p1 wool for p2 ore *)
let trade_1_output = ([ Wool; Brick; Ore; Wood ], [ Ore; Wool ])

(* trading p1 wool wool for p2 ore *)
let trade_2_output = ([ Brick; Wood; Ore ], [ Wool; Wool; Ore ])

(* trading p1 wood for p2 ore *)
let trade_3_output = ([ Wool; Wool; Brick; Ore ], [ Wood; Ore ])

(* trading p1 brick for p2 ore *)
let trade_4_output = ([ Wool; Wool; Ore; Wood ], [ Ore; Brick ])

(* trade p3 ore for p2 ore ore *)
let trade_5_output =
  ( [ Wheat; Wheat; Ore; Ore; Ore; Wool; Wool; Brick; Brick; Wood; Wood ],
    [ Ore ] )

(* p3 wheat for p1 wool wool *)
let trade_6_output =
  ( [ Wheat; Ore; Ore; Wool; Wool; Brick; Brick; Wood; Wood; Wool; Wool ],
    [ Brick; Wood; Wheat ] )

let bank1 =
  {
    num = 0;
    name = "Bank";
    color = White;
    cards =
      gen_cards [ Wool ] 19
      @ gen_cards [ Ore ] 19
      @ gen_cards [ Wood ] 20
      @ gen_cards [ Wheat ] 19
      @ gen_cards [ Brick ] 19;
    dev_cards = [];
    points = 0;
  }

(* p1 trades wood to the initial bank *)
let trade_bank_1_output = ([ Wool; Wool; Brick ], bank1.cards)

let bank2 =
  {
    num = 0;
    name = "Bank";
    color = White;
    cards =
      gen_cards [ Wool ] 18
      @ gen_cards [ Ore ] 21
      @ gen_cards [ Wood ] 19
      @ gen_cards [ Wheat ] 19
      @ gen_cards [ Brick ] 19;
    dev_cards = [];
    points = 0;
  }

(* p2 trades ore, ore to the initial bank in exchange for wool *)
let trade_bank_2_output = ([ Wool ], bank2.cards)

let bank3 =
  {
    num = 0;
    name = "Bank";
    color = White;
    cards =
      gen_cards [ Wool ] 19
      @ gen_cards [ Ore ] 17
      @ gen_cards [ Wood ] 19
      @ gen_cards [ Wheat ] 19
      @ gen_cards [ Brick ] 19;
    dev_cards = [];
    points = 0;
  }

(* p1 receives ore ore from initial bank *)
let trade_bank_3_output =
  ([ Wool; Wool; Brick; Wood; Ore; Ore ], bank3.cards)

let empty_rd : Adj_matrix.road = None

let p1_rd : Adj_matrix.road = Some 1

let p2_rd : Adj_matrix.road = Some 2

let p3_rd : Adj_matrix.road = Some 3

(* p1 builds [1,5] *)
let roads_1_output =
  let roads_init : Adj_matrix.road array array =
    Array.make_matrix 55 55 empty_rd
  in
  roads_init.(1).(5) <- p1_rd;
  roads_init.(5).(1) <- p1_rd;
  roads_init

(* p2 builds [2,6] after p1 builds [1,5] *)
let roads_2_output =
  let roads_init : Adj_matrix.road array array =
    Array.make_matrix 55 55 empty_rd
  in
  roads_init.(1).(5) <- p1_rd;
  roads_init.(5).(1) <- p1_rd;
  roads_init.(2).(6) <- p2_rd;
  roads_init.(6).(2) <- p2_rd;
  roads_init

(* p3 builds [51,54] after p1 builds [1,5] and p2 builds [2,6] *)
let roads_3_output =
  let roads_init : Adj_matrix.road array array =
    Array.make_matrix 55 55 empty_rd
  in
  roads_init.(1).(5) <- p1_rd;
  roads_init.(5).(1) <- p1_rd;
  roads_init.(2).(6) <- p2_rd;
  roads_init.(6).(2) <- p2_rd;
  roads_init.(51).(54) <- p3_rd;
  roads_init.(54).(51) <- p3_rd;
  roads_init

let p1_node : Adj_matrix.node =
  Some { player_num = 1; building = House }

let p2_node : Adj_matrix.node =
  Some { player_num = 2; building = House }

let p3_node : Adj_matrix.node = Some { player_num = 3; building = City }

let p4_node : Adj_matrix.node = Some { player_num = 4; building = City }

(* p1 builds a house at corner 1 on the board, equiv to being at index 1
   of the array *)
let corners_1_output =
  let empty_node : Adj_matrix.node = None in
  let corners_init : Adj_matrix.node array = Array.make 55 empty_node in
  corners_init.(1) <- p1_node;
  corners_init

(* p2 builds a house at corner 4 after p1 builds a house at corner 1 *)
let corners_2_output =
  let empty_node : Adj_matrix.node = None in
  let corners_init : Adj_matrix.node array = Array.make 55 empty_node in
  corners_init.(1) <- p1_node;
  corners_init.(4) <- p2_node;
  corners_init

(* p3 builds a city at corner 2 after p2 builds a house at corner 4 and
   p1 builds a house at corner 1 *)
let corners_3_output =
  let empty_node : Adj_matrix.node = None in
  let corners_init : Adj_matrix.node array = Array.make 55 empty_node in
  corners_init.(1) <- p1_node;
  corners_init.(4) <- p2_node;
  corners_init.(2) <- p3_node;
  corners_init

(* p4 builds a city at corner 54 after p3 builds a city at corner 2
   after p2 builds a house at corner 4 and p1 builds a house at corner 1 *)
let corners_4_output =
  let empty_node : Adj_matrix.node = None in
  let corners_init : Adj_matrix.node array = Array.make 55 empty_node in
  corners_init.(1) <- p1_node;
  corners_init.(4) <- p2_node;
  corners_init.(2) <- p3_node;
  corners_init.(54) <- p4_node;
  corners_init

(********************************************************************
  Start test suites
  ********************************************************************)

(* Trade between players *)
let trade_pl_tests =
  [
    trade_pl_test "p1 wool for p2 ore, res at start of list"
      (p1, [ Wool ])
      (p2, [ Ore ])
      trade_1_output;
    trade_pl_test "p1 wool wool for p2 ore"
      (p1, [ Wool; Wool ])
      (p2, [ Ore ])
      trade_2_output;
    trade_pl_test "p1 wood for p2 ore, res at end of list"
      (p1, [ Wood ])
      (p2, [ Ore ])
      trade_3_output;
    trade_pl_test "p1 brick for p2 ore, res in middle"
      (p1, [ Brick ])
      (p2, [ Ore ])
      trade_4_output;
    trade_pl_test "p3 ore for p2 ore ore"
      (p3, [ Ore ])
      (p2, [ Ore; Ore ])
      trade_5_output;
    trade_pl_test "p3 wheat for p1 wool wool"
      (p3, [ Wheat ])
      (p1, [ Wool; Wool ])
      trade_6_output;
  ]

(* Trade between player and the bank *)
let trade_bank_tests =
  [
    trade_bank_test "p1 wool to bank"
      (p1, [ Wood ])
      [] trade_bank_1_output;
    trade_bank_test "p2 ore ore for wool from bank"
      (p2, [ Ore; Ore ])
      [ Wool ] trade_bank_2_output;
    trade_bank_test "p1 receives ore ore from bank" (p1, [])
      [ Ore; Ore ] trade_bank_3_output;
  ]

(* Invalid trades *)
let trade_err_tests =
  [
    trade_pl_err_test "invalid: p1 trades no cards" (p1, [])
      (p2, [ Ore; Ore ])
      Player.InvalidTrade;
    trade_pl_err_test "invalid: p2 trades no cards"
      (p1, [ Ore ])
      (p2, []) Player.InvalidTrade;
    trade_pl_err_test "invalid: p1 insufficient res"
      (p1, [ Wool; Wool; Wool ])
      (p2, [ Ore ])
      Player.InvalidTrade;
    trade_pl_err_test "invalid: p2 insufficient res"
      (p1, [ Wool; Wool ])
      (p2, [ Wheat ])
      Player.InvalidTrade;
    trade_bank_err_test "invalid: p1 insuff res to trade to bank"
      (p1, [ Ore ])
      [] Player.InvalidTrade;
  ]

let roads_test =
  [
    update_roads_test "p1 builds road [1,5]" 1 1 5 roads_1_output;
    update_roads_test "p2 builds road [2,6]" 2 2 6 roads_2_output;
    update_roads_test "p3 builds road [51,54]" 3 51 54 roads_3_output;
    update_roads_err_test "fst bound too low" 1 0 2
      (Adj_matrix.InvalidRoad (0, 2));
    update_roads_err_test "snd bound too low" 1 1 0
      (Adj_matrix.InvalidRoad (1, 0));
    update_roads_err_test "both bounds too low" 1 ~-1 0
      (Adj_matrix.InvalidRoad (~-1, 0));
    update_roads_err_test "fst bound too high" 1 55 2
      (Adj_matrix.InvalidRoad (55, 2));
    update_roads_err_test "snd bound too high" 1 2 55
      (Adj_matrix.InvalidRoad (2, 55));
    update_roads_err_test "both bounds too high" 1 100 57
      (Adj_matrix.InvalidRoad (100, 57));
    update_roads_err_test "[1,5] already occupied" 1 1 5
      (Adj_matrix.OccupiedRoad (1, 5));
    (* TODO: add cases for nonexistent roads, e.g. [1,1], [1,2] *)
  ]

let corners_test =
  [
    update_corners_test "p1 builds house at corner 1" 1 House 1
      corners_1_output;
    update_corners_test "p2 builds house at corner 4" 2 House 4
      corners_2_output;
    update_corners_test "p3 builds city at corner 2" 3 City 2
      corners_3_output;
    update_corners_test "p4 builds city at corner 54" 4 City 54
      corners_4_output;
    update_corners_err_test "p1 builds house at corner 0" 1 House 0
      (Adj_matrix.InvalidTileId 0);
    update_corners_err_test "p1 builds city at corner 0" 1 City 0
      (Adj_matrix.InvalidTileId 0);
    update_corners_err_test "p2 builds house at corner 55" 2 House 55
      (Adj_matrix.InvalidTileId 55);
    update_corners_err_test "p2 builds city at corner 55" 2 City 55
      (Adj_matrix.InvalidTileId 55);
    update_corners_err_test "corner 1 already occupied" 2 House 1
      (Adj_matrix.OccupiedTileId 1);
    update_corners_err_test "corner 54 already occupied" 2 City 54
      (Adj_matrix.OccupiedTileId 54);
  ]

let suite =
  "test suite for building"
  >::: List.flatten
         [
           trade_pl_tests;
           trade_bank_tests;
           trade_err_tests;
           roads_test;
           corners_test;
         ]

let _ = run_test_tt_main suite
