val print_corner : Adj_matrix.node ->int->unit

val print_rd : Adj_matrix.road -> unit

val print_tile : Adj_matrix.tile -> unit

val line_1 : Adj_matrix.node -> Adj_matrix.node -> Adj_matrix.node -> int -> 
  int -> int -> unit

val line_2 : Adj_matrix.road -> Adj_matrix.road -> Adj_matrix.road -> 
  Adj_matrix.road -> Adj_matrix.road -> Adj_matrix.road -> unit

val line_3 : Adj_matrix.node -> Adj_matrix.node -> Adj_matrix.node -> 
  Adj_matrix.node -> int -> int -> int -> int -> unit

val line_4 : Adj_matrix.road -> Adj_matrix.road -> Adj_matrix.road -> 
  Adj_matrix.road -> Adj_matrix.tile -> Adj_matrix.tile -> Adj_matrix.tile -> 
  unit

val line_6 : Adj_matrix.road -> Adj_matrix.road -> Adj_matrix.road -> 
  Adj_matrix.road -> Adj_matrix.road -> Adj_matrix.road -> Adj_matrix.road -> 
  Adj_matrix.road -> unit

val line_7 : Adj_matrix.node -> Adj_matrix.node -> Adj_matrix.node -> 
  Adj_matrix.node -> Adj_matrix.node -> int -> int -> int -> int -> int -> unit

val line_8 : Adj_matrix.road -> Adj_matrix.road -> Adj_matrix.road -> 
  Adj_matrix.road -> Adj_matrix.road -> Adj_matrix.tile -> Adj_matrix.tile -> 
  Adj_matrix.tile -> Adj_matrix.tile -> unit

val line_10 : Adj_matrix.road -> Adj_matrix.road -> Adj_matrix.road -> 
  Adj_matrix.road -> Adj_matrix.road -> Adj_matrix.road -> Adj_matrix.road -> 
  Adj_matrix.road -> Adj_matrix.road -> Adj_matrix.road -> unit

val line_11 : Adj_matrix.node -> Adj_matrix.node -> Adj_matrix.node -> 
  Adj_matrix.node -> Adj_matrix.node -> Adj_matrix.node -> int -> int -> int -> 
  int -> int -> int -> unit

val line_12 : Adj_matrix.road -> Adj_matrix.road -> Adj_matrix.road -> 
  Adj_matrix.road -> Adj_matrix.road -> Adj_matrix.road -> Adj_matrix.tile -> 
  Adj_matrix.tile -> Adj_matrix.tile -> Adj_matrix.tile -> Adj_matrix.tile -> 
  unit

val print_board :  Adj_matrix.node array -> Adj_matrix.road array array -> 
  Adj_matrix.tile list -> unit
