(*generating board would create a list of 19 tiles, and assign resources
  and dice roll number to it*)

(* open Board open Player open State *)

(** [play_game f] starts the adventure in file [f]. *)
open Board

open Player
open Resource
open State
open Tile

exception Illegal

exception BadNumber

(* let board_default = "\n\ \ ( 1 )            ( 2 )\n\       ( A
   )     ( B )  ( C )    ( D )\n\    ( 3 )           ( 4 )          ( 5
   )\n\    ( E )           ( F )          ( G )\n\    ( 6 )           (
   7 )          ( 8 )\n\       ( H )    ( I )  ( J )    ( K )\n\
             ( 9 )            (10 )"

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
let board_default =
  "       ( 1 )            ( 2 )\n\
  \   ( A )     ( B )  ( C )    ( D )\n\
   ( 3 )           ( 4 )          ( 5 )\n\
   ( E )           ( F )          ( G )\n\
   ( 6 )           ( 7 )          ( 8 )\n\
  \   ( H )    ( I )  ( J )    ( K )\n\
  \       ( 9 )            (10 ) \n\n"

let parse (str : string) = failwith "TODO"

let end_game = print_endline "end game"

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

(* [print_board] prints out the updated board with new houses and roads *)
let print_board = print_string board_default

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
    print_string board_default;
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
    print_string board_default;
    setup players_list (num_players - 1) first_sec

(* [play_game num_pl pl_list] runs the rest of the game *)
let play_game num_pl =
  print_string "\nWelcome to Catan 3110. \n\n";
  let num =
    if num_pl = "4" then 4
    else if num_pl = "3" then 3
    else if num_pl = "2" then 2
    else 0
  in
  let players = create_player_list num num [] in
  print_string board_default;
  setup players (List.length players - 1) 1;
  setup players (List.length players - 1) 2

(* let pl = List.nth players 0 in let pl_name = pl.name in print_string
   pl_name; print_string ", where would you like to place a house? \n ";
   print_string "> " *)

(* need to cast num_pl into int here if num_pl = "4" then let num = 4
   else if num_pl = "3" then let num = 3 else if num_pl = "2" then let
   num = 2 *)

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
