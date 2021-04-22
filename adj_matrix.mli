type id = int

type dice_num = int

type resource = Resource.t

type resource_str = string

type corners = int list

type robber = bool

type tile = {
  id : id;
  dice_num : dice_num;
  resource : resource_str;
  corners : corners;
  robber : robber;
}

(** [from_json j] is the adventure that [j] represents. Requires: [j] is
    a valid JSON adventure representation. *)
val tiles_from_json : Yojson.Basic.t -> tile

val resource_from_string : string -> Resource.t
