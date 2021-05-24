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

(** [conn_with_road player v1 v2] returns true if the road denoted by
    [v1], [v2] is connected to a road owned by [player]. *)
let conn_with_road player v1 v2 =
  let has_connection = ref false in
  for i = 1 to 54 do
    match Adj_matrix.curr_roads.(i).(v2) with
    | None -> ()
    | Some n -> if n = player.num then has_connection := true else ()
  done;
  for j = 1 to 54 do
    match Adj_matrix.curr_roads.(v1).(j) with
    | None -> ()
    | Some n -> if n = player.num then has_connection := true else ()
  done;
  !has_connection

(** [conn_with_corner player v1 v2] returns true the road denoted by
    [v1], [v2] is connected to a corner of a tile that has one of
    [player]'s buildings. *)
let conn_with_corner player v1 v2 =
  let node1 = Adj_matrix.curr_corners.(v1) in
  let node2 = Adj_matrix.curr_corners.(v2) in
  match (node1, node2) with
  | Some { player_num = num; _ }, _ -> num = player.num
  | _, Some { player_num = num; _ } -> num = player.num
  | None, None -> false

(** [is_connected_rd player v1 v2] returns true if the road denoted by
    v1,v2 is connected to player [player]'s other houses, cities, or
    roads.

    Requires: v1 and v2 are in bounds of [1,54], and the road is not
    already occupied. *)
let is_connected_rd player v1 v2 =
  (* print_string ("\nplayer " ^ string_of_int player.num ^ " building
     rd on " ^ string_of_int v1 ^ ", " ^ string_of_int v2); print_string
     ("\n" ^ pp_array pp_node Adj_matrix.curr_corners); *)
  conn_with_road player v1 v2 || conn_with_corner player v1 v2

(** [check_road_list_aux player lst json] checks the validity of player
    [player] building a road denoted by a list of integers [lst], using
    [json] as the list of valid roads.

    Raises [Adj_matrix.InvalidRoadId (v1, v2)] if the road id is out of
    bounds. Raises [Adj_matrix.OccupiedRoad (v1, v2)] if the road is
    already occupied. Raises [Adj_matrix.RoadNotConnected (v1, v2)] if
    the road is not connected another road or corner owned by the
    player. *)
let check_road_list_aux (player : Player.t) (lst : int list) json =
  let valid_roads = Adj_matrix.roads_from_json json in
  if List.length lst <> 2 then raise RoadLength
  else
    match lst with
    | [ v1; v2 ] ->
        if
          (* check valid road ids *)
          (not (List.mem (v1, v2) valid_roads))
          && not (List.mem (v2, v1) valid_roads)
        then raise (InvalidRoadId (v1, v2))
        else if
          (* check road is unoccupied *)
          Adj_matrix.curr_roads.(v1).(v2) <> Adj_matrix.None
          && Adj_matrix.curr_roads.(v2).(v1) <> Adj_matrix.None
        then raise (Adj_matrix.OccupiedRoad (v1, v2))
        else if
          (* check road is connected *)
          not (is_connected_rd player v1 v2)
        then raise (Adj_matrix.RoadNotConnected (v1, v2))
        else "[" ^ string_of_int v1 ^ "," ^ string_of_int v2 ^ "]"
    | _ -> raise RoadLength

(** [check_road_input player str] returns the string [str] if it is
    valid for player [player] to build the road specified in [str],
    which is a string of the form ["\[v1,v2\]"], containing two indices
    that represent a road.

    [str] is valid if the road is not yet occupied, the road indices are
    in the bounds [1,54], and the road is represented in [json]. Also,
    the road a player builds must be connected to another road owned by
    that player or one of the houses/cities owned by that player.

    NOTE: for testing purposes, the [use_print] constant should be
    switched to [false].

    Ex. [check_road_input "\[0,2\]"] raises
    [Adj_matrix.InvalidRoadId (0,2)]

    Ex. [check_road_input "\[2,3,4\]"] raises [InvalidRoadFormat]

    Ex. [check_road_input "\[1,5\]"] returns ["\[1,5\]"] *)
let rec check_road_input (player : Player.t) (str : string) json :
    string =
  (* set [use_print] to false for testing *)
  let use_print = false in
  try
    check_road_list_aux player (Dev_card_logic.parse_road_str str) json
  with
  | RoadLength ->
      if use_print then
        (print_string
           "Please enter two points in the correct format. Format: \
            [*corner location*, *corner location*]\n";
         print_string "> ";
         check_road_input player (read_line ()))
          json
      else raise RoadLength
  | Adj_matrix.InvalidRoadId (v1, v2) ->
      if use_print then
        (print_string
           "Please enter two points within the range of [1,54]. \
            Format: [*corner location*, *corner location*]\n";
         print_string "> ";
         check_road_input player (read_line ()))
          json
      else raise (Adj_matrix.InvalidRoadId (v1, v2))
  | Adj_matrix.OccupiedRoad (v1, v2) ->
      if use_print then
        (print_string
           "Please enter the points of a road that is unoccupied. \
            Format: [*corner location*, *corner location*]\n";
         print_string "> ";
         check_road_input player (read_line ()))
          json
      else raise (Adj_matrix.OccupiedRoad (v1, v2))
  | Adj_matrix.RoadNotConnected (v1, v2) ->
      if use_print then
        (print_string
           "Please enter the points of a road that is connected to \
            your other roads, houses, or cities. Format: [*corner \
            location*, *corner location*]\n";
         print_string "> ";
         check_road_input player (read_line ()))
          json
      else raise (Adj_matrix.RoadNotConnected (v1, v2))
  | Dev_card_logic.InvalidRoadFormat ->
      if use_print then
        (print_string
           "Please write in the appropriate format. Format: [*corner \
            location*, *corner location*] ex: [1,4] \n";
         print_string "> ";
         check_road_input player (read_line ()))
          json
      else raise Dev_card_logic.InvalidRoadFormat

(** [check_corner_input index] returns corner index [i] if it is valid
    to build a house or city on that corner, i.e. it is in the bounds
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

(** [parse_quit str] quits the game if [str] is "QUIT" *)
let parse_quit = function
  | "QUIT" ->
      print_string "Thank you for playing Catan!";
      exit 0
  | _ -> ()
