type t =
  | Monopoly
  | Victory_Points
  | Road_Building
  | Year_Of_Plenty

let match_dev_string dev =
  match dev with
  | Monopoly -> "Monopoly"
  | Victory_Points -> "Victory_Points"
  | Road_Building -> "Road building"
  | Year_Of_Plenty -> "Year of Plenty"
