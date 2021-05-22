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

(** [check_has_no_road v1 v2] returns true if both [v1] and [v2] are
    both in the bounds [1,54] and the road matrix does not contain a
    road between [v1] and [v2], i.e. it is valid to build a road between
    those points. *)
let check_has_no_road v1 v2 =
  if
    v1 > 0 && v1 < 55 && v2 > 0 && v2 < 55
    && Adj_matrix.curr_roads.(v1).(v2) = None
    && Adj_matrix.curr_roads.(v2).(v1) = None
  then true
  else false
