open OUnit2
open Tile
open Player
open Resource

(* let cmp_set_like_lists lst1 lst2 = let uniq1 = List.sort_uniq compare
   lst1 in let uniq2 = List.sort_uniq compare lst2 in List.length lst1 =
   List.length uniq1 && List.length lst2 = List.length uniq2 && uniq1 =
   uniq2 *)

(* let t1 = make_tile "wheat" 6 1

   let t2 = make_tile "ore" 8 2

   let t1_edges = [ (1, 5); (5, 9); (9, 13); (8, 13); (4, 8); (1, 4) ]

   let t2_edges = [ (10, 14); (9, 14); (5, 9); (2, 5); (2, 6); (6, 10) ]

   let edges_test (name : string) (position : int) (expected_output :
   edge list) : test = name >:: fun _ -> assert_equal true
   (cmp_set_like_lists expected_output (edges_from_pos position))

   (* Test a tile's edges, neighbors, and presence of the robber *) let
   tile_attr_tests = [ (* edges_test "tile 1's edges" 1 t1_edges; *) (*
   edges_test "tile 2's edges" 2 t2_edges; *) (* TODO: test neighbors *)
   assert true; ] *)

(** [cmp_lists lst1 lst2] compares two lists. *)
let cmp_lists lst1 lst2 =
  let sorted1 = List.sort compare lst1 in
  let sorted2 = List.sort compare lst2 in
  sorted1 = sorted2

(** [cmp_tup_lists tup1 tup2] compares two tuples of resource lists.

    tup1 = (\[Ore; Wool; Wool; Brick\], \[Wood;Ore\]).

    tup2 = (\[Wool; Wool; Brick; Ore\], \[Ore;Wood\]), result of trade. *)
let cmp_tup_of_lists tup1 tup2 =
  let tup1_fst = List.sort compare (fst tup1) in
  let tup1_snd = List.sort compare (snd tup1) in
  let tup2_fst = List.sort compare (fst tup2) in
  let tup2_snd = List.sort compare (snd tup2) in
  tup1_fst = tup2_fst && tup1_snd = tup2_snd

let move_robber_tests = [ assert true ]

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

(** Printer for a tuple composed of two lists. *)
let pp_tup_list pp_elt lst =
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
  "([" ^ pp_elts (fst lst) ^ "], [" ^ pp_elts (snd lst) ^ "])"

let pp_string s = "\"" ^ s ^ "\""

let pp_resource = function
  | Wheat -> "Wheat"
  | Ore -> "Ore"
  | Wool -> "Wool"
  | Brick -> "Brick"
  | Wood -> "Wood"

(** [cards_from_trade trade_result num] returns the cards of one of the
    players in the tuple [trade_result], resulting from a trade. [num]
    denotes which of the players' cards we want, and it must be 1 or 2.

    Ex. A trade between players p1 and p2 results in (p1,p2). Then
    [cards_from_trade (p1, p2) 2] returns [p2.cards] *)
let cards_from_trade trade_result = function
  | 1 -> (fst trade_result).cards
  | 2 -> (snd trade_result).cards
  | _ -> failwith "There are only two players in a trade"

let trade_pl_test name trade1 trade2 expected_output =
  let trade_result = trade_to_player trade1 trade2 in
  let p1_cards = cards_from_trade trade_result 1 in
  let p2_cards = cards_from_trade trade_result 2 in
  let trade_result_cards = (p1_cards, p2_cards) in
  name >:: fun _ ->
  assert_equal expected_output ~cmp:cmp_tup_of_lists
    ~printer:(pp_tup_list pp_resource)
    trade_result_cards

let trade_err_test name trade1 trade2 expected_output =
  name >:: fun _ ->
  assert_raises expected_output (fun () ->
      trade_to_player trade1 trade2)

(** Constants to be used for testing *)
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

(** Test suites *)
let trade_tests =
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
    trade_err_test "invalid trade: p1 []" (p1, [])
      (p2, [ Ore; Ore ])
      Player.InvalidTrade;
    trade_err_test "invalid trade: p2 []"
      (p1, [ Ore ])
      (p2, []) Player.InvalidTrade;
  ]

let suite = "test suite for building" >::: List.flatten [ trade_tests ]

let _ = run_test_tt_main suite
