(** Representation of a Catan board. *)

(** The abstract type of values representing a Catan board. *)
type t

(* if a dice is rolled for player x: *)
(* 1. go through each player, and update their resource list based on if
   their tile number is the same as the dice roll *)
(* 2. x can choose to build a house/settlement by trading to a bank *)

(*build house at a certain corner of a tile. try to build house, throw
  error. Outputs a new board *)

(* Any json files we use for Catan boards will have its data/other
   functions in Board. *)

(** The type of a development card. *)
type dev_card

(** The type of a port. *)
type port

(** objects that player can trade cards into include dev cards,
    buildings *)
type trade_in
