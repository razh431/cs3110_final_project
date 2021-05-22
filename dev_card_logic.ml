open Player

let monopoly (pl : player) =
  (*calculating how many [res] cards are out there, named res_num *)
  print_string "What resource would you like to monopolize?";
  print_string ">";
  let res = Adj_matrix.resource_from_string (read_line ()) in
  let res_num =
    19
    - List.fold_left
        (fun acc x -> if x = res then acc + 1 else acc)
        0 bank.cards
  in
  let cards_generated = gen_cards [ res ] res_num in
  update_player pl cards_generated pl.dev_cards pl.points

let victory_points pl =
  update_player pl pl.cards pl.dev_cards (pl.points + 1)

let rec year_of_plenty pl : player =
  print_string "Which two resources would you like to get?";
  print_string "> ";
  let resources = Player.input_to_list (read_line ()) in
  if List.length resources = 2 then fst (trade_to_bank pl [] resources)
  else (
    print_string "Please only input two resources";
    print_string ">";
    year_of_plenty pl)

let knight pl = failwith "unimplemented"

let parse_road_str (s : string) =
  try
    s |> String.trim
    |> String.split_on_char '['
    |> String.concat ""
    |> String.split_on_char ']'
    |> String.concat ""
    |> String.split_on_char ','
    |> List.map String.trim |> List.map int_of_string
  with e ->
    print_string
      "\n\
       Please write in the appropriate format. Format: \n\
      \    [*corner location*, *corner location*] ex: [1,4] \n";
    exit 0

let road_building pl =
  print_string
    ", where would you like to build your first road? Format: [*corner \
     location*, *corner location*] ex: [1,4] \n\
    \ ";
  let road_loc = read_line () in
  let road_loc_list =
    parse_road_str road_loc |> List.map (fun x -> x - 1)
  in
  ignore
    (update_pl_roads pl.num
       (List.nth road_loc_list 0)
       (List.nth road_loc_list 1));
  pl

let rec dev_to_string (res_list : Dev_cards.t list) (acc : string) =
  match res_list with
  | [] -> acc
  | h :: t -> (
      match h with
      | Monopoly -> dev_to_string t ("Monopoly " ^ acc)
      | Victory_Points -> dev_to_string t ("Victory Points " ^ acc)
      | Knight -> dev_to_string t ("Knight " ^ acc)
      | Road_Building -> dev_to_string t ("Road building " ^ acc)
      | Year_Of_Plenty -> dev_to_string t ("Year of plenty " ^ acc))

let rm_used_dev dev_card (pl_cards_list : Dev_cards.t list) acc =
  failwith ""

let rec dev_logic dev_card pl : player =
  match dev_card with
  | "Monopoly" -> monopoly pl
  | "Victory_Points" -> victory_points pl
  | "Knight" -> failwith ""
  | "Road_Building" ->
      (*returns the same player*)
      road_building pl
  | "Year_Of_Plenty" -> year_of_plenty pl
  | _ ->
      print_string "Please choose one of your cards to use. ";
      print_string ">";
      dev_logic (read_line ()) pl

let dev_card_logic player : player =
  print_string
    (" You currently have "
    ^ dev_to_string player.dev_cards "."
    ^ "What would you like to use? \n ");
  print_string "> ";
  let dev_card = read_line () in
  dev_logic dev_card player
