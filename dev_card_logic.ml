open Player
open Parse

(* exception InvalidRoadFormat *)

let monopoly (player : player) =
  (*calculating how many [res] cards are out there, named res_num *)
  print_string "What resource would you like to monopolize? \n";
  print_string "> ";
  let res = Adj_matrix.resource_from_string (read_line ()) in
  let res_num =
    19
    - List.fold_left
        (fun acc x -> if x = res then acc + 1 else acc)
        0 bank.cards
  in
  let cards_generated = gen_cards [ res ] res_num in
  update_player player cards_generated player.dev_cards player.points

let victory_points player =
  update_player player player.cards player.dev_cards (player.points + 1)

let rec year_of_plenty player : player =
  print_string "Which two resources would you like to get? \n";
  print_string "> ";
  let resources = Player.input_to_list (read_line ()) in
  if List.length resources = 2 then
    fst (trade_to_bank player [] resources)
  else (
    print_string "Please only input two resources. \n";
    print_string "> ";
    year_of_plenty player)

let road_building player roads_json =
  let new_player = fst (trade_to_bank player [ Wood; Brick ] []) in
  let input = read_line () in
  if input = "QUIT" then exit 0
  else
    print_string
      ", where would you like to build your road? Format: [*corner \
       location*, *corner location*] ex: [1,4] \n\
      \ ";
  print_string "> ";
  let road_loc =
    Parse.check_road_input new_player (read_line ()) roads_json
  in
  let road_loc_list = Parse.parse_road_str road_loc in
  ignore
    (update_pl_roads new_player.num
       (List.nth road_loc_list 0)
       (List.nth road_loc_list 1)
       roads_json);
  new_player

let rec dev_to_string (res_list : Dev_cards.t list) (acc : string) =
  match res_list with
  | [] -> acc
  | h :: t -> (
      match h with
      | Monopoly -> dev_to_string t ("Monopoly " ^ acc)
      | Victory_Points -> dev_to_string t ("Victory Points " ^ acc)
      | Road_Building -> dev_to_string t ("Road building " ^ acc)
      | Year_Of_Plenty -> dev_to_string t ("Year of plenty " ^ acc))

let rec dev_logic dev_card player roads_json : player =
  match dev_card with
  | "Monopoly" -> monopoly player
  | "Victory_Points" -> victory_points player
  | "Road_Building" ->
      (*returns the same player*)
      road_building player roads_json
  | "Year_Of_Plenty" -> year_of_plenty player
  | _ ->
      print_string "Please choose one of your cards to use. \n";
      print_string "> ";
      dev_logic (read_line ()) player roads_json

(* [json] represents the json of valid roads *)
let dev_card_logic player roads_json =
  print_string
    (" You currently have "
    ^ dev_to_string player.dev_cards " ,"
    ^ "What would you like to use? \n");
  print_string "> ";
  let dev_card = read_line () in
  dev_logic dev_card player roads_json
