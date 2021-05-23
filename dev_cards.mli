type t =
  | Monopoly
  | Victory_Points
  | Road_Building
  | Year_Of_Plenty

val match_dev_string : t -> string
