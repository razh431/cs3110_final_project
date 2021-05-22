open Player

let monopoly (res : Resource.t) (pl : player) =
  (*calculating how many [res] cards are out there, named res_num *)
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

let year_of_plenty pl =
  print_string "Which two resources would you like to get?";
  print_string "> ";
  let resources = Player.input_to_list (read_line ()) in
  trade_to_bank pl resources

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
       (List.nth road_loc_list 1))
