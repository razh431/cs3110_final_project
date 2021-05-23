open Board
open Player
open Resource
open State
open Adj_matrix
open Print_board
open Dev_card_logic

exception Empty_Name

exception Repeated_Name

exception Letters_Name

exception RoadLength

let is_alpha = function
  | 'a' .. 'z' -> true
  | 'A' .. 'A' -> true
  | _ -> false

let name_player_helper n pl_list : string =
  match n with
  | "" -> raise Empty_Name
  | _ ->
      (*if not empty, check if it's a repeat of names*)
      let pl_names_list = List.map (fun x -> x.name) pl_list in
      if List.mem n pl_names_list then raise Empty_Name
      else
        let rec is_letter s =
          if not (is_alpha s.[0]) then raise Letters_Name
          else
            let l = String.length s in
            if l = 1 then n else is_letter (String.sub s 1 (l - 1))
        in
        is_letter n

let rec name_player n pl_list : string =
  try name_player_helper n pl_list with
  | Empty_Name ->
      print_string "Please choose a non empty name. ";
      print_string "> ";
      name_player (read_line ()) pl_list
  | Repeated_Name ->
      print_string "Please choose a name that hasn't been chosen. ";
      print_string "> ";
      name_player (read_line ()) pl_list
  | Letters_Name ->
      print_string "Please choose a name with only letters. ";
      print_string "> ";
      name_player (read_line ()) pl_list

(** [check_road_input str] returns the string [str] if it is valid to
    build the road specified in [str], which is a string of the form
    ["\[v1,v2\]"], containing two indices that represent a road. [str]
    is valid if the road is not yet occupied and if the road indices are
    in the bounds [1,54].

    Note: for testing purposes, the [use_print] constant can be switched
    to [false].

    Ex. [check_road_input "\[0,2\]"] raises
    [Adj_matrix.InvalidRoadId (0,2)]

    Ex. [check_road_input "\[2,3,4\]"] raises [InvalidRoadFormat]

    Ex. [check_road_input "\[1,5\]"] returns ["\[1,5\]"] *)
let rec check_road_input (str : string) : string =
  let check_has_no_road_aux (lst : int list) =
    if List.length lst <> 2 then raise RoadLength
    else
      match lst with
      | [ v1; v2 ] ->
          if
            (* check road in bounds *)
            v1 < 1 || v2 < 1 || v1 > 54 || v2 > 54
          then raise (Adj_matrix.InvalidRoadId (v1, v2))
          else if
            (* check road is unoccupied *)
            Adj_matrix.curr_roads.(v1).(v2) <> Adj_matrix.None
            || Adj_matrix.curr_roads.(v2).(v1) <> Adj_matrix.None
          then raise (Adj_matrix.OccupiedRoad (v1, v2))
          else "[" ^ string_of_int v1 ^ "," ^ string_of_int v2 ^ "]"
      | _ -> raise RoadLength
  in
  (* set [use_print] to false for testing *)
  let use_print = false in
  try check_has_no_road_aux (Dev_card_logic.parse_road_str str) with
  | RoadLength ->
      if use_print then (
        print_string
          "Please enter two points in the correct format. Format: \
           [*corner location*, *corner location*]\n";
        print_string "> ";
        check_road_input (read_line ()))
      else raise RoadLength
  | Adj_matrix.InvalidRoadId (v1, v2) ->
      if use_print then (
        print_string
          "Please enter two points within the range of [1,54]. Format: \
           [*corner location*, *corner location*]\n";
        print_string "> ";
        check_road_input (read_line ()))
      else raise (Adj_matrix.InvalidRoadId (v1, v2))
  | Adj_matrix.OccupiedRoad (v1, v2) ->
      if use_print then (
        print_string
          "Please enter the points of a road that is unoccupied. \
           Format: [*corner location*, *corner location*]\n";
        print_string "> ";
        check_road_input (read_line ()))
      else raise (Adj_matrix.OccupiedRoad (v1, v2))
  | Dev_card_logic.InvalidRoadFormat ->
      if use_print then (
        print_string
          "Please write in the appropriate format. Format: [*corner \
           location*, *corner location*] ex: [1,4] \n";
        print_string "> ";
        check_road_input (read_line ()))
      else raise Dev_card_logic.InvalidRoadFormat

(** [check_road_input index] returns corner index [i] if it is valid to
    build a house or city on that corner, i.e. it is in the bounds
    [1,54] and that index is not already occupied. *)
let rec check_corner_input index =
  let check_corner_input_aux idx =
    if idx < 1 || idx > 54 then raise (Adj_matrix.InvalidTileId idx)
    else if Adj_matrix.curr_corners.(idx) <> Adj_matrix.None then
      raise (Adj_matrix.OccupiedTileId idx)
    else idx
  in
  (* set [use_print] to false for testing *)
  let use_print = false in
  try check_corner_input_aux index with
  | Adj_matrix.InvalidTileId i ->
      if use_print then (
        print_string
          "Please enter the number of a corner in the range of [1,54]. \n";
        print_string "> ";
        check_corner_input (read_int ()))
      else raise (Adj_matrix.InvalidTileId i)
  | Adj_matrix.OccupiedTileId i ->
      if use_print then (
        print_string
          "Please enter the number of a corner in the range of [1,54] \
           that is not already occupied. \n";
        print_string "> ";
        check_corner_input (read_int ()))
      else raise (Adj_matrix.OccupiedTileId i)
  | _ ->
      print_string
        "Please enter a number of an unoccupied corner in the range of \
         [1,54]. \n";
      check_corner_input (read_int ())
