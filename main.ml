(*generating board would create a list of 19 tiles, and assign resources
  and dice roll number to it*)

(* open Board open Player open State *)

exception Illegal

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
  "\n\
  \  ( 1 )            ( 2 )\n\
  \       ( A\n\
  \   )     ( B )  ( C )    ( D )\n\
  \    ( 3 )           ( 4 )          ( 5\n\
  \   )\n\
  \    ( E )           ( F )          ( G )\n\
  \    ( 6 )           (\n\
  \   7 )          ( 8 )\n\
  \       ( H )    ( I )  ( J )    ( K )\n\
             ( 9 )            (10 )"

let parse (str : string) = failwith "TODO"
(*test change by Allison 3/30/21 12:30am*)
let end_game = print_endline "end game"

(* [create_player_list] returns a list of players depending on user
   inputs for the players names. [num_pl] is the number of players in
   that list *)
let rec create_player_list num_pl =
  let name_list = [] in
  print_string "Name of player";
  print_int num_pl;
  print_string ": \n";
  print_string "> ";
  let name_input = read_line () |> String.trim in
  (* append into list *)
  if num_pl - 1 >= 0 then create_player_list (num_pl - 1) else name_list

(* [play_game num_pl pl_list] runs the rest of the game *)
let play_game num_pl = print_endline "play game"

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
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the 3110 Catan game\n";
  print_endline "\nInstructions: Please enter the number of players 2-4";
  print_string "> ";
  let input_num_pl = read_line () |> String.trim in
  if num_pl_checker input_num_pl then play_game input_num_pl
  else inval_num_player input_num_pl

let () = main ()
