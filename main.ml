(*generating board would create a list of 19 tiles, and assign resources
  and dice roll number to it*)
(** [play_game f] starts the adventure in file [f]. *)

open Board
open Player
open Resource
open State
open Adj_matrix
open Print_board
open Dev_card_logic
open Parse
open Dev_cards

exception Illegal

exception BadNumber

(* [json] is of the abstract type t that represents our board *)
let json = Yojson.Basic.from_file "board.json"

(* [init_tiles tiles_from_json json] is the tiles list of json*)
let init_tiles = tiles_from_json json

(** [roads_json] is a list of tuples representing valid roads. *)
let roads_json = Yojson.Basic.from_file "roads.json"

let trade_prompt_str =
  "Type \"player\" to trade with player, type \"bank\" to build or get \
   a developement card, type \"use development card\" to use \
   developement cards, or type \"end turn\" to end turn.\n\
  \ >"

(** [create_player_list num_pl total_num_pl player_list] returns a list
    of players depending on user inputs for the players names. [num_pl]
    is the number of the player being added to the list of players.
    [total_num_pl] is the total number of players. *)
let rec create_player_list num_pl total_num_pl player_list =
  (* append into list *)
  if num_pl > 0 then begin
    (* let name_list = [] in *)
    let i = total_num_pl - num_pl + 1 in
    print_string "\nName of player ";
    print_int i;
    print_string ": \n";
    print_string "> ";
    let name_input = read_line () |> String.trim in
    let rec num_pl_setup str i =
      match str with
      | "HELP" | "RULES" | "QUIT" ->
          parse_help str;
          print_string "\nName of player ";
          print_int i;
          print_string ": \n";
          print_string "> ";
          let new_str = read_line () in
          num_pl_setup new_str i
      | _ ->
          let new_pl =
            init_player num_pl
              (name_player name_input player_list)
              Green
          in
          create_player_list (num_pl - 1) total_num_pl
            (new_pl :: player_list)
    in
    num_pl_setup name_input i
  end
  else player_list

(** [replace_players new_players old_player_list] replacing players in
    [new_players] with ones in [old_player] that correspond to the same
    number. For example, if [new_players] were [3'] and
    [old_player_list] was [1, 2, 3, 4] then the returning result should
    be [1, 2, 3', 4] *)
let replace_players new_player old_player_list : player list =
  let rec replace_helper player_list acc =
    match player_list with
    | [] ->
        (* List.iter (fun x -> print_string ("replace players: " ^
           x.name ^ ": " ^ "you current have " ^ unmatch_input x.cards "
           ," ^ " . \n")) *)
        List.rev acc
    | h :: t ->
        if h.num = new_player.num then
          replace_helper t (new_player :: acc)
        else replace_helper t (h :: acc)
  in
  replace_helper old_player_list []

(** [update_road_loc road_input player] updates the roads of the player
    [player] with the input [road_input]*)
let rec update_road_loc road_input player =
  let road_loc = Parse.check_road_input player road_input roads_json in
  let road_loc_list = parse_road_str road_loc in
  ignore
    (update_pl_roads player.num
       (List.nth road_loc_list 0)
       (List.nth road_loc_list 1)
       roads_json)

(** [get_house_loc house_input player] is the house location inputted by
    the user. If the input [house_input] by player [player] is not
    valid. Then the player will be promted to type in another error*)
let rec get_house_loc house_input player =
  let rec get_valid_house_loc str_num =
    print_string
      "This is not a valid input. Please enter a number of an \
       unoccupied corner in the range of [1,54]. \n";
    print_string "> ";
    let new_str = read_line () in
    try int_of_string new_str with _ -> get_valid_house_loc new_str
  in
  let house_num =
    try int_of_string house_input
    with _ -> get_valid_house_loc house_input
  in
  Parse.check_corner_input house_num

let rec loc_h str msg =
  match str with
  | "HELP" | "RULES" | "QUIT" ->
      parse_help str;
      print_string msg;
      loc_h (read_line ()) msg
  | _ -> str

(** [setup_home player first_sec] returns a new player after they have
    set up and built a home. [player] is the player building the home.
    [first_sec] is if they player is on the first or second stage of the
    setup*)

let setup_home player first_sec =
  let msg =
    if first_sec == 1 then
      ", where would you like to place your first house? \n  >"
    else ", where would you like to place your second house? \n "
  in
  print_string msg;
  let loc = read_line () in
  let house_num = get_house_loc (loc_h loc msg) curr_corners in
  let house_loc = Parse.check_corner_input house_num in
  let new_pl = distr_res_setup player house_loc json in
  ignore (update_pl_settlements new_pl.num House house_loc);
  print_board curr_corners curr_roads init_tiles;
  print_string player.name;
  print_string (", you currently have " ^ unmatch_input new_pl.cards "");
  new_pl

(** ([setup players_list num_players first_sec] is a new list of players
    after the players set up the game giving each player the ability to
    build a home and 2 roads twice. This is all before actual game play
    happens such as rolling dice. [players_list] is the list of players.
    [num_players] is the number of players in players_list. [first_sec]
    is 1 if we are on the first round of home building and 2 is if we
    are *)
let rec setup_road player first_sec =
  if first_sec == 1 then
    print_string
      "Where would you like to build your first road? Format: [*corner \
       location*, *corner location*] ex: [1,4] \n\
      \ "
  else (
    print_string
      ", where would you like to build your second road? Format: \
       [*corner location*, *corner location*] ex: [1,4]\n";
    print_string "> ");
  update_road_loc (read_line ()) player;
  print_board curr_corners curr_roads init_tiles

(* let rec setup players_list num_players first_sec : player list = if
   num_players < 0 && first_sec == 2 then ( print_string "Let's start
   the game! "; players_list) else if num_players < 0 && first_sec == 1
   then ( print_string "Now, we will build the second round of homes and
   roads! \n"; players_list) else let current_player = List.nth
   players_list num_players in let pl_name = current_player.name in
   print_string pl_name; let new_pl = setup_home current_player
   first_sec in let new_list = setup_road first_sec new_pl players_list
   in setup new_list (num_players - 1) first_sec *)

let rec setup players_list num_players first_sec : player list =
  if num_players < 0 && first_sec == 2 then (
    print_string "Let's start the game! ";
    players_list)
  else if num_players < 0 && first_sec == 1 then (
    print_string
      "Now, we will build the second round of homes and roads! \n";
    players_list)
  else
    let current_player = List.nth players_list num_players in
    let pl_name = current_player.name in
    print_string pl_name;
    let new_player = setup_home current_player first_sec in
    setup_road new_player first_sec;
    let new_list = replace_players new_player players_list in
    setup new_list (num_players - 1) first_sec

(** [player_trade player_list player] trades a resource between [player]
    and player_2 which the user inputs. player_list is the list of
    players *)
let player_trade player_list player =
  print_string "You can trade with these players: ";
  let other_players =
    List.filter (fun x -> x.name <> player.name) player_list
  in
  List.iter (fun x -> print_string (x.name ^ " ")) other_players;
  print_string
    "Please type the name of the player you would like to trade with, \
     or type \"back\" to go back.\n";
  print_string "> ";
  let rec other_h str =
    match str with
    | "HELP" | "RULES" | "QUIT" ->
        parse_help str;
        print_string
          "Please enter the name of a player, or type \t\"back\" to go \
           back.\n\
          \ > ";
        let new_str = read_line () in
        other_h new_str
    | _ -> str
  in
  let name = other_h (read_line ()) in
  if name = "back" then (player, List.nth player_list 1)
  else
    let player_2 =
      List.hd (List.filter (fun x -> x.name = name) other_players)
    in
    print_string (dev_to_string player_2.dev_cards ", ");
    trading_logic player player_2

(** [build_rd player json] builds updates the roads of the player
    [player] in the json [json]*)
let build_rd player json =
  try
    let new_player = fst (trade_to_bank player [ Wood; Brick ] []) in
    let input = read_line () in
    if input = "QUIT" then exit 0
    else
      print_string
        (player.name
       ^ ", where would you like to build your road? Format: [*corner \
          location*, *corner location*] ex: [1,4] \n\
         \ ");
    print_string ">";
    let road_loc =
      Parse.check_road_input new_player (read_line ()) roads_json
    in
    let road_loc_list = parse_road_str road_loc in
    ignore
      (update_pl_roads new_player.num
         (List.nth road_loc_list 0)
         (List.nth road_loc_list 1)
         roads_json);
    print_board curr_corners curr_roads init_tiles;
    new_player
  with InvalidTrade ->
    print_string "You do not have the valid resources for a road. ";
    player

(** [build_house player] builds a house at the node specified by the
    player [player]*)
let build_house player =
  try
    let new_player =
      fst (trade_to_bank player [ Wool; Brick; Wool; Wheat ] [])
    in
    print_string "Where would you like to place your house? \n ";
    print_string "> ";
    let input = read_line () in
    let rec house_setup str =
      match str with
      | "HELP" | "RULES" | "QUIT" ->
          parse_help str;
          print_string "Where would you like to place your house? \n ";
          print_string "> ";
          let new_str = read_line () in
          house_setup new_str
      | _ ->
          (* read value and print out changed board *)
          let house_loc =
            Parse.check_corner_input (int_of_string str)
          in
          ignore (update_pl_settlements new_player.num House house_loc);
          print_board curr_corners curr_roads init_tiles;
          print_string new_player.name;
          { new_player with points = new_player.points + 1 }
    in
    house_setup input
  with InvalidTrade ->
    print_string "You do not have the valid resources for a house. ";
    player

(** [build_city player] builds a city based on where which node the
    player [player] specifies to build the city*)
let build_city player =
  try
    let new_player =
      fst (trade_to_bank player [ Wheat; Wheat; Ore; Ore; Ore ] [])
    in
    print_string "Where would you like to place your city? \n ";
    print_string "> ";
    (* read value and print out changed board *)
    let city_loc = read_line () in
    let rec city_setup str =
      match str with
      | "HELP" | "RULES" | "QUIT" ->
          parse_help str;
          print_string "Where would you like to place your city? \n ";
          print_string "> ";
          let new_str = read_line () in
          city_setup new_str
      | _ -> (
          try
            let city_loc = int_of_string str in
            ignore (update_pl_settlements new_player.num City city_loc);
            print_board curr_corners curr_roads init_tiles;
            print_string new_player.name;
            { new_player with points = new_player.points + 1 }
          with Failure _ ->
            print_string "Please put an integer input. \n > ";
            let new_str = read_line () in
            city_setup new_str)
    in
    city_setup city_loc
  with InvalidTrade ->
    print_string "You do not have the valid resources for a city. ";
    player

(** [create_dev player] creates a dev card associated to the player
    [player]*)
let create_dev player : player =
  Random.self_init ();
  try
    let new_player =
      fst (trade_to_bank player [ Wool; Wheat; Ore ] [])
    in
    let dev_cards_list =
      [ Monopoly; Victory_Points; Road_Building; Year_Of_Plenty ]
    in
    let new_card = List.nth dev_cards_list (Random.int 4) in
    print_string ("You received a " ^ match_dev_string new_card ^ "!");
    { new_player with dev_cards = new_card :: new_player.dev_cards }
  with InvalidTrade -> (
    print_string
      "You do not have valid resources for a development card \n";
    print_string "> ";
    print_string
      " What resource would you like to trade in? Must have 4 of this \
       resource! ";
    print_string "> ";
    let input = read_line () in
    let rec input_list str =
      match str with
      | "HELP" | "RULES" | "QUIT" ->
          parse_help str;
          print_string
            "What resource would you like to trade in? Must have 4 of \
             this resource! \n\
            \ ";
          print_string "> ";
          let input = read_line () in
          input_list input
      | str -> str
    in
    let res = input_to_list (input_list input) in
    let res_in = gen_cards res 4 in
    print_string " What would you like to trade for? \n ";
    print_string "> ";
    let res_out = input_list (read_line ()) |> input_to_list in
    try
      let new_player = fst (trade_to_bank player res_in res_out) in
      print_string
        ("Your cards now: " ^ unmatch_input new_player.cards "");
      new_player
    with InvalidTrade ->
      print_string
        "You do not have the valid resources to trade with the bank";
      player)

(** [input_list str] helps with checking if str is "HELP", "RULES", or
    "QUIT". If so, parse_help is called *)
let rec input_list str =
  match str with
  | "HELP" | "RULES" | "QUIT" ->
      parse_help str;
      print_string
        "What resource would you like to trade in? Must have 4 of this \
         resource! \n\
        \ ";
      print_string "> ";
      let input = read_line () in
      input_list input
  | str -> str

(** [trade_4_1_card player] allows a player [player] to trade four of a
    kind of resource for another resource. *)
let trade_4_1_card player : player =
  print_string "> ";
  print_string
    " What resource would you like to trade in? Must have 4 of this \
     resource! ";
  print_string "> ";
  let input = read_line () in
  let res = input_to_list (input_list input) in
  let res_in = gen_cards res 4 in
  print_string " What would you like to trade for? \n ";
  print_string "> ";
  let res_out = input_list (read_line ()) |> input_to_list in
  let new_pl = fst (trade_to_bank player res_in res_out) in
  print_string ("Your cards now: " ^ unmatch_input new_pl.cards "");
  new_pl

(** [build_from_input build_type] builds the specified type from the
    [build_type] input *)
let build_from_input (build_type : string) player =
  let to_lower = String.lowercase_ascii build_type in
  match to_lower with
  | "card" -> (
      try trade_4_1_card player
      with Player.InvalidTrade ->
        print_string "Cannot build. Please enter another command. ";
        player)
  | "road" -> (
      try build_rd player json
      with Player.InvalidTrade ->
        print_string "Cannot build. Please enter another command. ";
        player)
  | "house" -> (
      try build_house player
      with Player.InvalidTrade ->
        print_string "Cannot build. Please enter another command. ";
        player)
  | "city" -> (
      try build_city player
      with Player.InvalidTrade ->
        print_string "Cannot build. Please enter another command.";
        player)
  | "developement card" -> (
      try create_dev player
      with Player.InvalidTrade ->
        print_string "Cannot build. Please enter another command. ";
        player)
  | _ -> player

(** [bank_trade players_list player] allows the player [player] of the
    players_list [players_list] their resources for a Road, House, City,
    Developement card, or Resource Cards *)
let rec bank_trade (players_list : player list) (player : player) :
    player * player list =
  print_string
    "Please type \"road\", \"settlement\", \"city\", or \"developement \
     card\" to build.  Type \"back\" to go back. \n\
    \ Road: 1 wood, 1 brick\n\
    \ House: 1 wood, 1 brick, 1 wool, 1 wheat\n\
    \ City: 2 wheat, 3 ore\n\
    \ Development card: 1 Wool, 1 wheat, 1 ore \n\
    \ Card: 4 of any resource to 1 card\n";
  print_string "> ";
  let build_type_s = read_line () in
  let rec build_h str =
    match str with
    | "HELP" | "RULES" | "QUIT" ->
        parse_help str;
        print_string
          "Please type \"road\", \"settlement\", \"city\", or \
           \"developement card\" to build.  Type \"back\" to go back. \n\
          \ Road: 1 wood, 1 brick\n\
          \ House: 1 wood, 1 brick, 1 wool, 1 wheat\n\
          \ City: 2 wheat, 3 ore\n\
          \ Development card: 1 Wool, 1 wheat, 1 ore \n\
          \ Card: 4 of any resource to 1 card\n";
        print_string "> ";
        let new_str = read_line () in
        build_h new_str
    | "back" -> (player, players_list)
    | _ ->
        let new_player = build_from_input build_type_s player in
        let new_player_list = replace_players new_player players_list in
        (new_player, new_player_list)
  in
  build_h build_type_s

(* [roll_dice] is a random integer 1-12 *)
let rec roll_dice = Random.int 12 + 1

(** [use_dev_card] is the new player list after *)
let use_dev_card player player_list roads_json =
  let new_player = dev_card_logic player roads_json in
  let new_list = replace_players new_player player_list in
  (new_player, new_list)

(* [trade_main player_list player roads_json] is the new player list
   after the player has chosen to trade with player, trade with bank,
   use resource cards, or end turn*)
let rec trade_main player_list player roads_json =
  print_string
    (player.name ^ " , you currently have "
    ^ unmatch_input player.cards " . \n");
  print_string trade_prompt_str;
  let input2 = read_line () in
  match input2 with
  | "player" -> (
      try
        let pl_trade_tup = player_trade player_list player in
        let new_player_list =
          replace_players (fst pl_trade_tup) player_list
          |> replace_players (snd pl_trade_tup)
        in
        trade_main new_player_list (fst pl_trade_tup) roads_json
      with InvalidTrade ->
        print_string
          "You do not have sufficient resources to trade with the bank";
        trade_main player_list player roads_json)
  | "bank" -> (
      try
        let tup = bank_trade player_list player in
        let new_player = fst tup in
        let new_player_list = snd tup in
        trade_main new_player_list new_player roads_json
      with InvalidTrade ->
        print_string
          "You do not have sufficient resources to trade with the bank";
        trade_main player_list player roads_json)
  | "use developement card" -> (
      try
        let tup = use_dev_card player player_list roads_json in
        let new_player = fst tup in
        let new_player_list = snd tup in
        trade_main new_player_list new_player roads_json
      with InvalidTrade ->
        print_string
          "You do not have sufficient resources to trade with the bank";
        trade_main player_list player roads_json)
  | "end turn" -> player_list
  | "HELP" ->
      parse_help "HELP";
      player_list
  | "RULES" ->
      parse_help "RULES";
      player_list
  | "QUIT" ->
      parse_help "QUIT";
      player_list
  | _ -> trade_main player_list player roads_json

(** [play_turn player] is a new player list updated after the specified
    player [player] has gone. First, they roll a dice and everyone gets
    their resources, then [player] can choose to trade with players,
    trade with bank, or end turn. The function ends when they select end
    turn. *)
let rec play_turn players_list player json roads_json =
  Random.self_init ();
  print_string player.name;
  print_string ", type \"roll\" to roll dice\n > ";
  (* Todo: parse input *)
  let input = read_line () in
  if input = "roll" then begin
    print_string "Rolling...\n";
    let num = Random.int 11 + 2 in
    print_string ("Rolled " ^ string_of_int num ^ "!\n");
    let new_player_list = distr_res players_list num json in
    let new_player =
      List.hd
        (List.filter (fun p -> p.name = player.name) new_player_list)
    in
    let new_list = replace_players new_player players_list in
    trade_main new_list new_player roads_json
  end
  else if input = "HELP" || input = "RULES" || input = "QUIT" then (
    parse_help input;
    play_turn players_list player json roads_json)
  else play_turn players_list player json roads_json

(** [play_turns players_list] will continuously carry out the turns of
    each player until someone has 10 victory points, meaning they have
    won the game *)
let rec play_turns
    (players_list : player list)
    (player : player)
    n
    json
    roads_json =
  let num_players = List.length players_list in
  if player.points = 10 then (
    print_string player.name;
    print_string " has won the game. Congratulations!")
  else
    let player_list_new_list =
      play_turn players_list player json roads_json
    in
    let new_n = (n + 1) mod num_players in
    print_int new_n;
    print_string "new n ^ \n";
    (* new_n represents next player*)
    play_turns player_list_new_list
      (List.nth players_list new_n)
      (n + 1) json roads_json

(** [play_game num_pl json roads_json] runs the main functionality of
    the game-creating a players list, printing the board, setting up the
    game, and playing the turns until a player wins. At any point, the
    user can choose to QUIT the game by typing "QUIT" *)
let play_game num_pl json roads_json =
  if num_pl = "QUIT" then (
    print_string "Thank you for playing OCatan.";
    exit 0)
  else
    let num =
      if num_pl = "4" then 4
      else if num_pl = "3" then 3
      else if num_pl = "2" then 2
      else 0
    in
    print_string "\nPlease name the players. \n\n";
    let players = create_player_list num num [] in
    print_board curr_corners curr_roads init_tiles;
    let new_list = setup players (List.length players - 1) 1 in
    let new_list_2 =
      List.rev (setup new_list (List.length players - 1) 2)
    in
    play_turns new_list_2 (List.hd new_list_2) 0 json roads_json

(** [num_pl_checker input_num_pl] is the boolean value of if
    [input_num_players] is a valid int of players. *)
let rec num_pl_checker input_num_pl =
  String.equal input_num_pl "2"
  || String.equal input_num_pl "3"
  || String.equal input_num_pl "4"
  || String.equal input_num_pl "QUIT"

(** [inval_num_player input json roads_json] is called when a user
    enters an invalid number of players [input]. *)
let rec inval_num_player inval_input json roads_json =
  print_string "Your input ";
  print_string inval_input;
  print_string " is not valid. \n Please enter 2, 3, or 4. \n";
  print_string "> ";
  let input_num_pl = read_line () in
  match input_num_pl with
  | "HELP" | "RULES" | "QUIT" -> parse_help input_num_pl
  | _ ->
      if num_pl_checker input_num_pl then
        play_game input_num_pl json roads_json
      else inval_num_player input_num_pl json roads_json

(** [main] runs the beginning of the game that asks for the input for
    the number of players and asks you to input another number if the
    number is not between 2-4 *)
let main () =
  (* ANSITerminal.print_string [ ANSITerminal.red ] *)
  print_string "\n\nWelcome to the OCatan!\n";
  print_endline "\nInstructions: Please enter the number of players 2-4";
  print_string "> ";
  let input_num_pl = read_line () |> String.trim in
  let rec num_pl_setup str =
    match str with
    | "HELP" | "RULES" | "QUIT" ->
        parse_help str;
        print_string "Please enter the number of players 2-4\n > ";
        let new_str = read_line () in
        num_pl_setup new_str
    | _ ->
        if num_pl_checker str then play_game str json roads_json
        else inval_num_player str json roads_json
  in
  num_pl_setup input_num_pl

let () = main ()
