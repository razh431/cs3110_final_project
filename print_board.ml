open Adj_matrix

type slash =
  | Forward
  | Backward
  | Straight

let print_corner (corner : node) (num : int) =
  let strnum = Int.to_string num in
  let head =
    if String.length strnum = 1 then "( " ^ strnum else "(" ^ strnum
  in
  print_string head;
  match corner with
  | None -> print_string " )"
  | Some t ->
      (* t is the settlement *)
      let build = match t.building with House -> "H" | City -> "C" in
      begin
        match t.player_num with
        | 1 -> ANSITerminal.print_string [ ANSITerminal.blue ] build
        | 2 -> ANSITerminal.print_string [ ANSITerminal.red ] build
        | 3 -> ANSITerminal.print_string [ ANSITerminal.green ] build
        | 4 -> ANSITerminal.print_string [ ANSITerminal.yellow ] build
        | 5 -> ANSITerminal.print_string [ ANSITerminal.white ] build
        | 6 -> ANSITerminal.print_string [ ANSITerminal.blue ] build
        | _ -> failwith "this should never happen printcorner"
      end;
      print_string ")"

let print_rd rd (s : slash) =
  let s_type =
    match s with Forward -> "/" | Backward -> "\\" | Straight -> "|"
  in
  match rd with
  | Some t -> (
      match t with
      | 1 -> ANSITerminal.print_string [ ANSITerminal.blue ] s_type
      | 2 -> ANSITerminal.print_string [ ANSITerminal.red ] s_type
      | 3 -> ANSITerminal.print_string [ ANSITerminal.green ] s_type
      | 4 -> ANSITerminal.print_string [ ANSITerminal.yellow ] s_type
      | 5 -> ANSITerminal.print_string [ ANSITerminal.white ] s_type
      | 6 -> ANSITerminal.print_string [ ANSITerminal.blue ] s_type
      | _ -> failwith "this should never happen printrd")
  | None -> (
      match s with
      | Forward -> print_string s_type
      | Backward -> print_string s_type
      | Straight -> print_string s_type)

let print_tile tile =
  if tile.robber = true then print_string "ROBBER"
  else
    let strnum = Int.to_string tile.dice_num in
    let head =
      if String.length strnum = 1 then " " ^ strnum else strnum
    in
    print_string head;
    match tile.resource with
    | Wool -> print_string " wol"
    | Ore -> print_string " ore"
    | Wood -> print_string " wod"
    | Brick -> print_string " brk"
    | Wheat -> print_string " wht"

let print_corner_row corners start size =
  print_string "\n   ";
  let rec print_init_space x =
    match x < 6 with
    | true ->
        print_string "       ";
        print_init_space (x + 1)
    | false -> ()
  in
  print_init_space size;
  for i = 0 to size - 1 do
    print_corner corners.(start + i) (start + i);
    print_string "         "
  done

let print_road_row roads n start size =
  (* size is 3 4 5 *)
  print_string "\n        ";
  let rec print_init_space x =
    match size with
    | 4 -> print_string "       "
    | 3 -> print_string "              "
    | _ -> ()
  in
  print_init_space size;
  for i = 0 to size - 1 do
    if n < 12 then begin
      (* top half *)
      print_rd roads.(start + i).(start + i + size) Forward;
      print_string "       ";
      print_rd roads.(start + i).(start + i + size + 1) Backward;
      print_string "     "
    end
    else begin
      print_rd roads.(start + i).(start + i - size - 1) Backward;
      print_string "       ";
      print_rd roads.(start + i).(start + i - size) Forward;
      print_string "     "
    end
  done

let print_tile_row roads tiles size t_start rd_start =
  print_string "\n     ";
  let rec print_init_space x =
    match x < 5 with
    | true ->
        print_string "       ";
        print_init_space (x + 1)
    | false -> ()
  in
  print_init_space size;
  for i = 0 to size - 1 do
    print_rd roads.(rd_start + i).(rd_start + i + size + 1) Straight;
    print_string "   ";
    print_tile (List.nth tiles (t_start + i));
    print_string "    "
  done;
  print_rd roads.(rd_start + size).(rd_start + size + size + 1) Straight

let print_board
    (corners : node array)
    (roads : road array array)
    (tiles : tile list) =
  print_corner_row corners 1 3;
  print_road_row roads 2 1 3;
  print_corner_row corners 4 4;
  print_tile_row roads tiles 3 0 4;
  print_corner_row corners 8 4;
  print_road_row roads 6 8 4;
  print_corner_row corners 12 5;
  print_tile_row roads tiles 4 3 12;
  print_corner_row corners 17 5;
  print_road_row roads 10 17 5;
  print_corner_row corners 22 6;
  print_tile_row roads tiles 5 7 22;
  print_corner_row corners 28 6;
  print_road_row roads 14 34 5;
  print_corner_row corners 34 5;
  print_tile_row roads tiles 4 12 34;
  print_corner_row corners 39 5;
  print_road_row roads 18 44 4;
  print_corner_row corners 44 4;
  print_tile_row roads tiles 3 16 44;
  print_corner_row corners 48 4;
  print_road_row roads 22 52 3;
  print_corner_row corners 52 3;
  print_string "\n\n"
