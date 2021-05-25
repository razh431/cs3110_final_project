type slash =
  | Forward
  | Backward
  | Straight

val print_color : int -> string -> unit

val print_corner : Adj_matrix.node -> int -> unit

val print_rd : Adj_matrix.road -> slash -> unit

val print_tile : Adj_matrix.tile -> unit

val print_corner_row : Adj_matrix.node array -> int -> int -> unit

val print_road_row :
  Adj_matrix.road array array -> int -> int -> int -> unit

val print_tile_row :
  Adj_matrix.road array array ->
  Adj_matrix.tile list ->
  int ->
  int ->
  int ->
  unit

val board_top :
  Adj_matrix.node array ->
  Adj_matrix.road array array ->
  Adj_matrix.tile list ->
  unit

val board_bot :
  Adj_matrix.node array ->
  Adj_matrix.road array array ->
  Adj_matrix.tile list ->
  unit

val print_board :
  Adj_matrix.node array ->
  Adj_matrix.road array array ->
  Adj_matrix.tile list ->
  unit
