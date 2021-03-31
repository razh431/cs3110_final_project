open OUnit2
open Player
open Resource

(* let exception_commands_test n input out = n >:: fun _ ->
   assert_raises out (fun _ -> parse input) *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

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

let _ = run_test_tt_main suite
