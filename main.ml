(*generating board would create a list of 19 tiles, and assign resources
  and dice roll number to it*)

(* open Board open Player open State *)
(** [play_game f] starts the adventure in file [f]. *)
open Board

open Player
open Resource
open State
open Adj_matrix

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
  "                        ( 1 )           ( 2 )          ( 3 )\n\
  \                    ( A )     ( B ) ( C )    ( D ) ( E )    ( F )\n\
  \                 ( 4 )           ( 5 )          ( 6 )          ( 7 )\n\
  \                 ( G )           ( H )          ( I )          ( J )\n\
  \                 ( 8 )           ( 9 )          (10 )          (11 )\n\
  \              ( K ) ( L )     ( M ) ( N )    ( O ) ( P )    ( Q ) ( \
   R )\n\
  \          (12 )          (13 )          (14 )          (15 \
   )          (16 )\n\
  \          ( S )          ( T )          ( U )          ( V \
   )          ( W )\n\
  \          (17 )          (18 )          (19 )          (20 \
   )          (21 )\n\
  \      ( X )   ( Y ) ( Z )     (AA ) (AB )    (AC ) (AD )    (AE )  \
   (AF )    (AG )   \n\
  \   (22 )         (23 )           (24 )          (25 )          (26 \
   )          (27 )\n\
  \   (AH )         (AI )           (AJ )          (AK )          (AL \
   )          (AM )\n\
  \   (28 )         (29 )           (30 )          (31 )          (32 \
   )          (33 )\n\
  \      (AN )   (AO ) (AP )     (AQ ) (AR )    (AS ) (AT )    (AU ) \
   (AV )    (AW )\n\
  \           (34 )         (35 )          (36 )          (37 \
   )          (38 ) \n\
  \           (AX )         (AY )          (AZ )          (BA \
   )          (BB )\n\
  \           (39 )         (40 )          (41 )          (42 \
   )          (43 )\n\
  \              (BC ) (BD )     (BE ) (BF )    (BG ) (BH )    (BI ) \
   (BJ )\n\
  \                 (44 )           (45 )          (46 )          (47 )\n\
  \                 (BK )           (BL )          (BM )          (BN )\n\
  \                 (48 )           (49 )          (50 )          (51 )\n\
  \                    (BO )     (BP ) (BQ )    (BR ) (BS )    (BT )\n\
  \                         (52 )           (53 )          (54 ) \n\
  \ \n"

let parse (str : string) = failwith "TODO"

let end_game = print_endline "end game"

(* [create_player_list] returns a list of players depending on user
   inputs for the players names. [num_pl] is the number of players in
   that list *)

let rec create_player_list num_pl total_num_pl pl_list =
  (* let name_list = [] in *)
  print_string "\nName of player ";
  print_int (total_num_pl - num_pl + 1);
  print_string ": \n";
  print_string "> ";
  let name_input = read_line () |> String.trim in
  (* append into list *)
  if num_pl > 1 then
    let new_pl = init_player num_pl name_input Green in
    create_player_list (num_pl - 1) total_num_pl (new_pl :: pl_list)
  else pl_list

let rec unmatch_input (res_list : Resource.t list) (acc : string) =
  match res_list with
  | [] -> acc
  | h :: t ->
      if h = Wool then unmatch_input t ("Wool " ^ acc)
      else if h = Ore then unmatch_input t ("Ore " ^ acc)
      else if h = Wood then unmatch_input t ("Wood " ^ acc)
      else if h = Brick then unmatch_input t ("Brick " ^ acc)
      else if h = Wheat then unmatch_input t ("Wheat " ^ acc)
      else failwith "incorrect command"

let rec matching_input
    (input_filtered : string list)
    (acc : Resource.t list) =
  match input_filtered with
  | [] | [ "" ] -> acc
  | h :: t ->
      matching_input t (Adj_matrix.resource_from_string h :: acc)

let input_to_list input =
  (*input string into list of string words*)
  (*todo: fix spaces*)
  let filtered_input = input |> String.split_on_char ' ' in

  (* |> List.filter (fun l -> l <> "") in List.filter (fun s -> s <> "")
     filtered_input in *)
  matching_input filtered_input []

(* [play_game num_pl pl_list] runs the rest of the game *)
let play_game num_pl =
  print_string "\nStarting game. \n\n";
  let num =
    if num_pl = "4" then 4
    else if num_pl = "3" then 3
    else if num_pl = "2" then 2
    else 0
  in
  let players = create_player_list num num [] in
  print_string board_default;
  (* let pl = List.nth_opt players 1 in let n = pl.name in print_string
     n; print_string " Where would you like to place a house? \n ";
     print_string "> "; *)
  print_string
    (" You currently have "
    ^ unmatch_input player1.cards ""
    ^ "What would you like to trade? \n ");
  print_string "> ";
  let trade1 = (player1, input_to_list (read_line ())) in
  print_string " What would you like to trade for? \n ";
  print_string "> ";
  let trade2 = (player2, input_to_list (read_line ())) in
  let player_1 = fst (trade_to_player trade1 trade2) in
  print_string ("Your cards now: " ^ unmatch_input player_1.cards "")

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
