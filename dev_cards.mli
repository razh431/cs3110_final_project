(** t is the type of dev cards*)
type t =
  | Monopoly
  | Victory_Points
  | Road_Building
  | Year_Of_Plenty

(** [match_dev_string dev_card ] returns the [dev_card] in a string
    format*)
val match_dev_string : t -> string
