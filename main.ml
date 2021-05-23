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

(* [board_default] is the default board the game will be played on and
   will only be 2 tiles for now*)

let json = Yojson.Basic.from_file "board.json"

(* [board_default] is the default board the game will be played on and
   will only be 2 tiles for now*)
let init_tiles = tiles_from_json json

let parse (str : string) = failwith "TODO"

(* [create_player_list num_pl total_num_pl pl_list] returns a list of
   players depending on user inputs for the players names. [num_pl] is
   the number of the player being added to the list of players.
   [total_num_pl] is the total number of players. *)

let rec create_player_list num_pl total_num_pl pl_list =
  (* append into list *)
  if num_pl > 0 then (
    (* let name_list = [] in *)
    print_string "\nName of player ";
    print_int (total_num_pl - num_pl + 1);
    print_string ": \n";
    print_string "> ";
    let name_input = read_line () |> String.trim in
    let new_pl = init_player num_pl name_input Green in
    create_player_list (num_pl - 1) total_num_pl (new_pl :: pl_list))
  else pl_list

(*[replace_players new_players old_player_list] replacing players in
  [new_players] with ones in [old_player] that correspond to the same
  number. For example, if [new_players] were [3'] and [old_player_list]
  was [1, 2, 3, 4] then the returning result should be [1, 2, 3', 4] *)
let replace_players new_player old_player_list : player list =
  let rec replace_helper pl_list acc =
    match pl_list with
    | [] ->
        List.iter
          (fun x ->
            print_string
              ("replace players: " ^ x.name ^ ": " ^ "you current have "
              ^ unmatch_input x.cards " ,"
              ^ " . \n"))
          acc;

        List.rev acc
    | h :: t ->
        if h.num = new_player.num then
          replace_helper t (new_player :: acc)
        else replace_helper t (h :: acc)
  in
  replace_helper old_player_list []

(* [setup players_list num_players first_sec] is a new list of players
   after the players set up the game giving each player the ability to
   build a home and 2 roads twice. This is all before actual game play
   happens such as rolling dice[players_list] is the list of players.
   [num_players] is the number of players in players_list. [first_sec]
   is 1 if we are on the first round of home building and 2 is if we are
   on the second round of home building.*)
let rec setup players_list num_players first_sec : player list =
  if num_players < 0 && first_sec == 2 then (
    print_string "Let's start the game! ";
    let new_list = players_list in
    new_list)
  else if num_players < 0 && first_sec == 1 then (
    print_string
      "Now, we will build the second round of homes and roads! \n";
    let new_list = players_list in
    new_list)
  else
    let pl = List.nth players_list num_players in
    let pl_name = pl.name in
    print_string pl_name;
    if first_sec == 1 then
      print_string
        ", where would you like to place your first house? \n "
    else
      print_string
        ", where would you like to place your second house? \n ";
    print_string "> ";
    (* read value and print out changed board *)
    let house_loc = Parse.check_corner_input (read_int ()) in
    let new_pl = distr_res_setup pl house_loc json in
    ignore (update_pl_settlements new_pl.num House house_loc);
    print_board curr_corners curr_roads init_tiles;
    print_string pl_name;
    print_string (" You currently have " ^ unmatch_input new_pl.cards "");
    if first_sec == 1 then (
      print_string
        ", where would you like to build your first road? Format: \
         [*corner location*, *corner location*] ex: [1,4] \n\
        \ ";
      print_string "> ")
    else (
      print_string
        ", where would you like to build your second road? Format: \
         [*corner location*, *corner location*] ex: [1,4]\n";
      print_string "> ");
    (*todo: factor building road logic out*)
    let road_loc = Parse.check_road_input (read_line ()) in
    let road_loc_list = parse_road_str road_loc in
    ignore
      (update_pl_roads new_pl.num
         (List.nth road_loc_list 0)
         (List.nth road_loc_list 1));
    (* if curr_roads.(List.nth road_loc_list 0).(List.nth road_loc_list
       1) != None then print_string "there is a road here"; *)
    print_board curr_corners curr_roads init_tiles;
    let new_list = replace_players new_pl players_list in
    setup new_list (num_players - 1) first_sec

(* TODO: Change to list.filter *)
(* [get_player list name] is the player in the player list [list] with
   the name [name]*)
let rec get_player list name =
  match list with
  | h :: t -> if h.name == name then h else get_player t name
  | [] ->
      print_string
        " ,please type the name of the player you would like to trade \
         with.\n";
      print_string "> ";
      let player_2 = read_line () in
      get_player list player_2

(* [player_trade pl_list player ] trades a resource between [player] and
   player_2 which the user inputs. pl_list is the list of players *)
let player_trade pl_list player =
  print_string "You can trade with these players: ";
  (* TODO: print everything in the list except the current player *)
  (* TODO: print a space and comma between each player *)
  let other_players =
    List.filter (fun x -> x.name <> player.name) pl_list
  in
  List.iter (fun x -> print_string (x.name ^ " ")) other_players;
  print_string
    "Please type the name of the player you would like to trade with.\n";
  print_string "> ";
  let name = read_line () in
  let player_2 =
    List.hd (List.filter (fun x -> x.name = name) other_players)
  in
  print_string (dev_to_string player_2.dev_cards ", ");
  (* get_player pl_list name (* TODO: make sure trading logic checks if
     the resource inputted is valid *) in *)
  trading_logic player player_2

let build_rd player =
  let new_pl = fst (trade_to_bank player [ Wood; Brick ] []) in
  let road_loc = read_line () in
  let road_loc_list = parse_road_str road_loc in
  ignore
    (update_pl_roads new_pl.num
       (List.nth road_loc_list 0)
       (List.nth road_loc_list 1));
  new_pl

let build_house player =
  let new_pl =
    fst (trade_to_bank player [ Wool; Brick; Wool; Wheat ] [])
  in
  print_string "Where would you like to place your house? \n ";
  print_string "> ";
  (* read value and print out changed board *)
  let house_loc = Parse.check_corner_input (read_int ()) in
  ignore (update_pl_settlements new_pl.num House house_loc);
  print_board curr_corners curr_roads init_tiles;
  print_string new_pl.name;
  new_pl

let build_city player =
  let new_pl =
    fst (trade_to_bank player [ Wheat; Wheat; Ore; Ore; Ore ] [])
  in
  print_string "Where would you like to place your city? \n ";
  print_string "> ";
  (* read value and print out changed board *)
  let city_loc = read_int () in
  ignore (update_pl_settlements new_pl.num City city_loc);
  print_board curr_corners curr_roads init_tiles;
  print_string new_pl.name;
  new_pl

let create_dev player : player =
  let new_pl = fst (trade_to_bank player [ Wool; Wheat; Ore ] []) in
  let dev_cards_list =
    [ Monopoly; Victory_Points; Road_Building; Year_Of_Plenty ]
  in
  let new_card = List.nth dev_cards_list (Random.int 4) in
  print_string ("You received a " ^ match_dev_string new_card ^ "!");
  { new_pl with dev_cards = new_card :: new_pl.dev_cards }

let build_from_input (build_type : string) player =
  match build_type with
  | "road" -> (
      try build_rd player
      with Player.InvalidTrade ->
        print_string
          " Sorry, you do not have sufficient resources to build a road";
        player)
  | "house" -> (
      try build_house player
      with Player.InvalidTrade ->
        print_string
          " Sorry, you do not have sufficient resources to build a \
           house";
        player)
  | "city" -> (
      try build_city player
      with Player.InvalidTrade ->
        print_string
          " Sorry, you do not have sufficient resources to build a city";
        player)
  | "developement card" -> (
      try create_dev player
      with Player.InvalidTrade ->
        print_string
          " Sorry, you do not have sufficient resources to build a \
           development card";
        player)
  | _ -> player

let rec bank_trade (players_list : player list) (player : player) :
    player list =
  (* TODO: Use parse to parse through the input*)
  print_string
    "Please type \"road\", \"settlement\", \"city\", or \"developement \
     card\" to build \n\
    \ Road: 1 wood, 1 brick\n\
    \ House: 1 wood, 1 brick, 1 wool, 1 wheat\n\
    \ City: 2 wheat, 3 ore\n\
    \ Developement Card: 1 Wool, 1 wheat, 1 ore";
  print_string "> ";
  let build_type_s = read_line () in
  let new_pl = build_from_input build_type_s player in
  replace_players new_pl players_list

(* [roll_dice] is a random integer 1-12 *)
let rec roll_dice = Random.int 12 + 1

(* [use_dev_card] is the new player list after *)
let use_dev_card player player_list =
  let new_player = dev_card_logic player in
  replace_players new_player player_list

(* init_player (number : int) (pl_name : string) (col : color) num =
   number; name = pl_name; color = col; cards = []; dev_cards = [];
   points = 0; *)

(* [trade pl_list player] is the new player list after the player has
   chosen to trade with player, trade with bank, use resource cards, or
   end turn*)
let rec trade_main pl_list player =
  print_string
    (player.name ^ " , you currently have "
    ^ unmatch_input player.cards " ."
    ^ " . \n");
  print_string
    "Type \"player\" to trade with player, type \"bank\" to build or \
     get a \n\
     developement card, type \"use development card\" to use \
     developement cards, or type \"end turn\" to end turn.\n";
  print_string "> ";
  let input2 = read_line () in
  match input2 with
  | "player" ->
      let pl_trade_tup = player_trade pl_list player in
      let new_pl_list =
        replace_players (fst pl_trade_tup) pl_list
        |> replace_players (fst pl_trade_tup)
      in
      trade_main new_pl_list player
  | "bank" ->
      let new_pl_list = bank_trade pl_list player in
      trade_main new_pl_list player
  | "use developement card" ->
      let new_pl_list = use_dev_card player pl_list in
      trade_main new_pl_list player
  | "end turn" -> pl_list
  (* TODO: Figure out how to quit the game *)
  | "QUIT" -> exit 0
  | _ -> trade_main pl_list player

(* [play_turn player] is a new player list updated after the specified
   player [player] has gone. First, they roll a dice and everyone gets
   their resources, then [player] can choose to trade with players,
   trade with bank, or end turn. The function ends when they select end
   turn. *)
let rec play_turn players_list player json =
  Random.self_init ();
  print_string player.name;
  print_string ", type \"roll\" to roll dice\n > ";
  (* Todo: parse input *)
  let input = read_line () in
  if input = "roll" then begin
    print_string "rolling...\n";
    let num = Random.int 12 + 1 in
    print_string ("rolled " ^ string_of_int num ^ "!\n");
    let new_pl_list = distr_res players_list num json in
    let new_pl =
      List.hd (List.filter (fun p -> p.name = player.name) new_pl_list)
    in
    List.iter
      (fun x ->
        print_string
          ("play turn dist resource--  player: " ^ x.name ^ ": "
         ^ "you current have "
          ^ unmatch_input x.cards " ,"
          ^ " . \n"))
      new_pl_list;
    trade_main new_pl_list new_pl
  end
  else play_turn players_list player json

(* [play_turns players_list] will continuously carry out the turns of
   each player until someone has 10 victory points, meaning they have
   won the game *)
let rec play_turns (players_list : player list) (player : player) n json
    =
  let num_players = List.length players_list in
  if player.points = 10 then (
    print_string player.name;
    print_string "\n   has won the game. Congratulation!")
  else if n >= num_players then
    let pl_list_new_list = play_turn players_list player json in
    play_turns pl_list_new_list
      (List.nth players_list 0)
      num_players json
  else
    let pl_list_new_list = play_turn players_list player json in
    let new_n = n + 1 in
    play_turns pl_list_new_list (List.nth players_list new_n) new_n json

(* [play_game num_pl pl_list] runs the rest of the game *)
let play_game num_pl json =
  print_string "\nWelcome to Catan 3110. \n\n";
  let num =
    if num_pl = "4" then 4
    else if num_pl = "3" then 3
    else if num_pl = "2" then 2
    else 0
  in
  let players = create_player_list num num [] in
  print_board curr_corners curr_roads init_tiles;

  let new_list = setup players (List.length players - 1) 1 in
  let new_list_2 = setup new_list (List.length players - 1) 2 in
  play_turns new_list_2 (List.hd players) 0 json

(* Distribute resources *)
(* let pl = List.nth players 0 in let pl_name = pl.name in print_string
   pl_name; print_string ", where would you like to place a house? \n ";
   print_string "> " *)

(* let pl = List.nth_opt players 1 in let n = pl.name in print_string n;
   print_string " Where would you like to place a house? \n ";
   print_string "> "; *)
(* print_string (" You currently have " ^ unmatch_input player1.cards ""
   ^ "What would you like to trade? \n "); print_string "> "; let trade1
   = (player1, input_to_list (read_line ())) in print_string " What
   would you like to trade for? \n "; print_string "> "; let trade2 =
   (player2, input_to_list (read_line ())) in let player_1 = fst
   (trade_to_player trade1 trade2) in print_string ("Your cards now: " ^
   unmatch_input player_1.cards "") *)

(* [num_pl_checker input_num_players] is the boolean value of if
   [input_num_players] is a valid int of players. *)
let rec num_pl_checker input_num_pl =
  String.equal input_num_pl "2"
  || String.equal input_num_pl "3"
  || String.equal input_num_pl "4"

(* [inval_num_player str_input] is called when a user enters an invalid
   number of players [inval_input] *)
let rec inval_num_player inval_input json =
  print_string "Your input ";
  print_string inval_input;
  print_string " is not valid. \n Please enter 2, 3, or 4. \n";
  print_string "> ";
  let input_num_pl = read_line () in
  if num_pl_checker input_num_pl then play_game input_num_pl json
  else inval_num_player input_num_pl json

(* [main] runs the beginning of the game that asks for the input for the
   number of players and asks you to input another number if the number
   is not between 2-4 *)
let main () =
  (* ANSITerminal.print_string [ ANSITerminal.red ] *)
  print_string "\n\nWelcome to the 3110 Catan game\n";
  print_endline "\nInstructions: Please enter the number of players 2-4";
  print_string "> ";
  let input_num_pl = read_line () |> String.trim in
  if num_pl_checker input_num_pl then play_game input_num_pl json
  else inval_num_player input_num_pl json

let () = main ()
