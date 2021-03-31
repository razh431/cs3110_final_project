open OUnit2
open Tile

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists. That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates. Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

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

let _ = run_test_tt_main suite
