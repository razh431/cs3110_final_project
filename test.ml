open OUnit2
open Tile
open Player
open Resource

(* let exception_commands_test n input out = n >:: fun _ ->
   assert_raises out (fun _ -> parse input) *)
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

let cmp_lists lst1 lst2 =
  let sorted1 = List.sort compare lst1 in
  let sorted2 = List.sort compare lst2 in
  sorted1 = sorted2

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

(** [trade_p1_test name trade1 trade2 expected_output] checks if the
    first player has the correct cards after the cards specified in
    [trade1] are traded away and the cards specified in [trade2] are
    received by player 1. *)
let trade_p1_test name trade1 trade2 expected_output =
  let trade_result = trade_to_player trade1 trade2 in
  let p1_cards = cards_from_trade trade_result 1 in
  name >:: fun _ ->
  assert_equal ~cmp:cmp_lists ~printer:(pp_list pp_resource) p1_cards
    (fst expected_output)

(** [trade_p2_test name trade1 trade2 expected_output] checks if the
    second player has the correct cards after the cards specified in
    [trade2] are traded away and the cards specified in [trade1] are
    received by player 2. *)
let trade_p2_test name trade1 trade2 expected_output =
  let trade_result = trade_to_player trade1 trade2 in
  let p2_cards = cards_from_trade trade_result 2 in
  name >:: fun _ ->
  assert_equal ~cmp:cmp_lists ~printer:(pp_list pp_resource) p2_cards
    (snd expected_output)

(** Constants to be used for testing *)
let p1 = { (init_player 1 "a" Blue) with cards = [ Wool; Wool; Brick ] }

let p2 = { (init_player 2 "b" Red) with cards = [ Ore; Ore ] }

let trade_1_output = ([ Wool; Brick; Ore ], [ Ore; Wool ])

let trade_2_output = ([ Brick; Ore ], [ Wool; Wool; Ore ])

(** Testing suites *)
let trade_tests =
  [
    trade_p1_test "one item trade, player 1"
      (p1, [ Wool ])
      (p2, [ Ore ])
      trade_1_output;
    trade_p2_test "one item trade, player 2"
      (p1, [ Wool ])
      (p2, [ Ore ])
      trade_1_output;
    trade_p1_test "two item trade,  player 1"
      (p1, [ Wool; Wool ])
      (p2, [ Ore ])
      trade_2_output;
    trade_p2_test "two item trade,  player 2"
      (p1, [ Wool; Wool ])
      (p2, [ Ore ])
      trade_2_output;
  ]

let suite = "test suite for building" >::: List.flatten [ trade_tests ]

let _ = run_test_tt_main suite
