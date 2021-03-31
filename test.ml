open OUnit2
<<<<<<< HEAD
open Tile

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists. That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates. Second, they must contain the same elements,
    though not necessarily in the same order. *)
=======
open Player
open Resource

(* let exception_commands_test n input out = n >:: fun _ ->
   assert_raises out (fun _ -> parse input) *)
>>>>>>> player
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

<<<<<<< HEAD
let t1 = make_tile "wheat" 6 1

let t2 = make_tile "ore" 8 2

let t1_edges = [ (1, 5); (5, 9); (9, 13); (8, 13); (4, 8); (1, 4) ]

let t2_edges = [ (10, 14); (9, 14); (5, 9); (2, 5); (2, 6); (6, 10) ]

let edges_test
    (name : string)
    (position : int)
    (expected_output : edge list) : test =
  name >:: fun _ ->
  assert_equal true
    (cmp_set_like_lists expected_output (edges_from_pos position))

(* Test a tile's edges, neighbors, and presence of the robber *)
let tile_attr_tests =
  [
    edges_test "tile 1's edges" 1 t1_edges;
    edges_test "tile 2's edges" 2 t2_edges;
    (* TODO: test neighbors *)
  ]

let move_robber_tests = []

let suite =
  "test suite for Tile"
  >::: List.flatten [ tile_attr_tests; move_robber_tests ]
=======
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

let rachel = make_player

let rachel_traded = make_player_a

let mindy = make_player_1

let mindy_traded = make_player_1a

let trade_to_player_test n trade1 trade2 out =
  n >:: fun _ -> assert_equal (trade_to_player trade1 trade2) out

(* (let room_ids_test n adv out = n >:: fun _ -> assert_equal
   ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string) (room_ids adv)
   out) *)
let player_tests =
  [
    trade_to_player_test "2 item trade"
      (rachel, [ Wood; Wool ])
      (mindy, [ Brick ])
      (rachel_traded, mindy_traded);
  ]

let suite = "test suite for building" >::: List.flatten [ player_tests ]
>>>>>>> player

let _ = run_test_tt_main suite
