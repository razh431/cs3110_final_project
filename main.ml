(*generating board would create a list of 19 tiles, and assign resources
  and dice roll number to it*)

(* open Board open Player open State *)

(** [play_game f] starts the adventure in file [f]. *)
open Board

open Player
open Resource
open State
open Adj_matrix
open Print_board

exception Illegal

exception BadNumber

(* 
   let play_game num_pl pl_list = failwith "TODO"

   let rec num_pl_checker = print_string "\n\ Sorry, the number of
   players inputted is invalid.\n\ \ Please enter 2, 3, or 4. \n";
   print_string "> "

   let input_num_pl = read_line ()

   let check_input input_num_pl = if String.equal input_num_pl "2" ||
   String.equal input_num_pl "3" || String.equal input_num_pl "4" then
   num_pl_checker else play_game input_num_pl [] *)

(** [main ()] prompts for the game to play, then starts it. *)

(* let main () = ANSITerminal.print_string [ ANSITerminal.red ]
   "\n\nWelcome to the 3110 Catan game\n"; print_endline
   "\nInstructions: Please enter the number of players 2-4";
   print_string "> "; let input_num_pl = read_line () in if
   (input_num_pl < 2) OR (input_num_pl > 4) then num_pl_checker else
   play_game input_num_pl []

   let () = main () *)

(* [board_default] is the default board the game will be played on and
   will only be 2 tiles for now*)
let init_tiles = tiles_from_json (Yojson.Basic.from_file "board.json") 
  
let parse (str : string) = failwith "TODO"

(* [create_player_list num_pl total_num_pl pl_list] returns a list of
   players depending on user inputs for the players names. [num_pl] is
   the number of the player being added to the list of players.
   [total_num_pl] is the total number of players. *)

(* TODO: check if players have the same name *)
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



(* [place_home loc] places a home down at the specified [loc] *)
(* let place_home (loc: string) = loc *)

(* [setup players_list num_players first_sec] sets up the game before
   players start rolling dice by giving each player the ability to build
   a home and 2 roads twice. [players_list] is the list of players.
   [num_players] is the number of players in players_list. [first_sec]
   is 1 if we are on the first round of home building and 2 is if we are
   on the second round of home building.*)
let rec setup players_list num_players first_sec =
  if num_players < 0 && first_sec == 2 then
    print_string "Let's start the game!"
  else if num_players < 0 && first_sec == 1 then
    print_string
      "Now, we will build the second round of homes and roads! \n"
  else
    let pl = List.nth players_list num_players in
    let pl_name = pl.name in
    print_string pl_name;
    (* print_string (string_of_int n); *)
    if first_sec == 1 then
      print_string
        ", where would you like to place your first house? \n "
    else
      print_string
        ", where would you like to place your second house? \n ";
    print_string "> \n";
    (* read value and print out changed board *)
    print_board init_corners init_road_mtx init_tiles;
    print_string pl_name;
    if first_sec == 1 then
      print_string
        ", where would you like to build your first two roads? Format: \
         [1,4] [4,7] \n\
        \ "
    else
      print_string
        ", where would you like to build your second two roads? \
         Format: [1,4] [4,7] \n\
        \ ";
    print_string "> \n";
    (* read value and print out changed board *)
    (* let loc = read_line () in place_home loc *)
    print_board init_corners init_road_mtx init_tiles;
    setup players_list (num_players - 1) first_sec

(* TODO: figure out what happens if the random int selected is 0 *)
(* [roll_dice] is a random integer 1-12 *)
let rec roll_dice = Random.int 13

(* [distr_res players_list rum] is the new players_list with distributed
   resources to all players in [players_list] based on the num rolled by
   the dice [num]*)
(* let distr_res players_list num = failwith "TODO" *)

(* [player_trade res_list1 res_list2] trades resources*)
(* let player_trade = failwith "TODO" *)

(* [play_turn player] will be the turn for the specified player in the
   argument [player]. First, they roll a dice and everyone gets their
   resources, then player whos turn it is can choose to trade with
   players, trade with bank, or end turn. The function ends when they
   select end turn. *)
(* let play_turn players_list player = print_string "Type \"roll\" to
   roll dice"; let input = read_line () in if input = "roll" then let
   num = roll_dice in let new_list = distr_res players_list num in let
   rec trade player= print_string "Type \"player\" to trade with player,
   type \"bank\" to trade with resource, or type \"end turn\" to end
   turn."; let input2 = read_line () in if input2 = "player" then let
   new_player = player_trade in trade new_player else if input2 = "bank"
   then let new_player = bank_trade in trade new_player else if input =
   "end" (* else type valid input *)

   else print_string "Please type roll to roll dice" *)

(* [play_turns players_list] will continuously carry out the turns of
   each player until someone has 10 victory points, meaning they have
   won the game *)
(* let rec play_turns players_list (player : player) n = if
   player.points == 10 then ( print_string player.name; print_string "
   has won the game. Congradulations!") else if n == 0 then let
   num_players = List.length players_list in play_turns players_list
   (List.nth players_list num_players) num_players else ( play_turn
   players_list player; let new_n = n + 1 in play_turns players_list
   (List.nth players_list new_n) new_n) *)

(* [play_game num_pl pl_list] runs the rest of the game *)
let play_game num_pl =
  print_string "\nWelcome to Catan 3110. \n\n";

  (* let rec unmatch_input (res_list : Resource.t list) (acc : string) =
     match res_list with | [] -> acc | h :: t -> if h = Wool then
     unmatch_input t ("Wool " ^ acc) else if h = Ore then unmatch_input
     t ("Ore " ^ acc) else if h = Wood then unmatch_input t ("Wood " ^
     acc) else if h = Brick then unmatch_input t ("Brick " ^ acc) else
     if h = Wheat then unmatch_input t ("Wheat " ^ acc) else failwith
     "incorrect command"

     let rec matching_input (input_filtered : string list) (acc :
     Resource.t list) = match input_filtered with | [] | [ "" ] -> acc |
     h :: t -> matching_input t (Adj_matrix.resource_from_string h ::
     acc)

     let input_to_list input = (*input string into list of string
     words*) (*todo: fix spaces*) let filtered_input = input |>
     String.split_on_char ' ' in

     (* |> List.filter (fun l -> l <> "") in List.filter (fun s -> s <>
     "") filtered_input in *) matching_input filtered_input [] *)
  let num =
    if num_pl = "4" then 4
    else if num_pl = "3" then 3
    else if num_pl = "2" then 2
    else 0
  in
  let players = create_player_list num num [] in
  print_board init_corners init_road_mtx init_tiles;

  setup players (List.length players - 1) 1;
  setup players (List.length players - 1) 2

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
let rec inval_num_player inval_input =
  print_string "Your input ";
  print_string inval_input;
  print_string " is not valid. \n Please enter 2, 3, or 4. \n";
  print_string "> ";
  let input_num_pl = read_line () in
  if num_pl_checker input_num_pl then play_game input_num_pl
  else inval_num_player input_num_pl

(* [main] runs the beginning of the game that asks for the input for the
   number of players and asks you to input another number if the number
   is not between 2-4 *)
let main () =
  (* ANSITerminal.print_string [ ANSITerminal.red ] *)
  print_string "\n\nWelcome to the 3110 Catan game\n";
  print_endline "\nInstructions: Please enter the number of players 2-4";
  print_string "> ";
  let input_num_pl = read_line () |> String.trim in
  if num_pl_checker input_num_pl then play_game input_num_pl
  else inval_num_player input_num_pl

let () = main ()
