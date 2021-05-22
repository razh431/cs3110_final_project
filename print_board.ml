open Adj_matrix

type slash = Forward | Backward | Straight

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

let print_rd rd (s:slash)=
let s_type = begin match s with 
| Forward -> "/"
| Backward -> "\\"
| Straight -> "|"
    end
  in match rd with
  | Some t -> (
      match t with
      | 1 -> ANSITerminal.print_string [ ANSITerminal.blue ] s_type
      | 2 -> ANSITerminal.print_string [ ANSITerminal.red ] s_type
      | 3 -> ANSITerminal.print_string [ ANSITerminal.green ] s_type
      | 4 -> ANSITerminal.print_string [ ANSITerminal.yellow ] s_type
      | 5 -> ANSITerminal.print_string [ ANSITerminal.white ] s_type
      | 6 -> ANSITerminal.print_string [ ANSITerminal.blue ] s_type
      | _ -> failwith "this should never happen printrd")
  | None -> 
    match s with 
    | Forward -> print_string s_type
    | Backward -> print_string s_type
    | Straight -> print_string s_type

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

(* 
let line_1 corn1 corn2 corn3 c1 c2 c3 =
  print_string "                        ";
  ignore (print_corner corn1 c1);
  print_string "         ";
  ignore (print_corner corn2 c2);
  print_string "         ";
  ignore (print_corner corn3 c3);
  print_string "\n"

let line_2 rd1 rd2 rd3 rd4 rd5 rd6 d1 d2 =
  print_string "                      ";
  print_rd rd1 d1;
  print_string "       ";
  print_rd rd2 d2;
  print_string "     ";
  print_rd rd3 d1;
  print_string "       ";
  print_rd rd4 d2;
  print_string "     ";
  print_rd rd5 d1;
  print_string "       ";
  print_rd rd6 d2;
  print_string "\n"

let line_3 corn4 corn5 corn6 corn7 c4 c5 c6 c7 =
  print_string "                 ";
  ignore (print_corner corn4 c4);
  print_string "         ";
  ignore (print_corner corn5 c5);
  print_string "         ";
  ignore (print_corner corn6 c6);
  print_string "         ";
  ignore (print_corner corn7 c7);
  print_string "\n"

let line_4 rd1 rd2 rd3 rd4 t1 t2 t3 =
  print_string "                   ";
  print_rd rd1 Straight;
  print_string "   ";
  print_tile t1;
  print_string "    ";
  print_rd rd2 Straight;
  print_string "   ";
  print_tile t2;
  print_string "    ";
  print_rd rd3 Straight;
  print_string "   ";
  print_tile t3;
  print_string "    ";
  print_rd rd4 Straight;
  print_string "\n"

let line_6 rd1 rd2 rd3 rd4 rd5 rd6 rd7 rd8 d1 d2 =
  print_string "                ";
  print_rd rd1 d1;
  print_string "     ";
  print_rd rd2 d2;
  print_string "       ";
  print_rd rd3 d1;
  print_string "     ";
  print_rd rd4 d2;
  print_string "       ";
  print_rd rd5 d1;
  print_string "     ";
  print_rd rd6 d2;
  print_string "       ";
  print_rd rd7 d1;
  print_string "     ";
  print_rd rd8 d2;
  print_string "\n"

let line_7 corn12 corn13 corn14 corn15 corn16 c12 c13 c14 c15 c16 =
  print_string "          ";
  ignore (print_corner corn12 c12);
  print_string "         ";
  ignore (print_corner corn13 c13);
  print_string "         ";
  ignore (print_corner corn14 c14);
  print_string "         ";
  ignore (print_corner corn15 c15);
  print_string "         ";
  ignore (print_corner corn16 c16);
  print_string "\n"

let line_8 rd1 rd2 rd3 rd4 rd5 t1 t2 t3 t4 =
  print_string "            ";
  print_rd rd1 Straight;
  print_string "   ";
  print_tile t1;
  print_string "    ";
  print_rd rd2 Straight;
  print_string "   ";
  print_tile t2;
  print_string "    ";
  print_rd rd3 Straight;
  print_string "   ";
  print_tile t3;
  print_string "    ";
  print_rd rd4 Straight;
  print_string "   ";
  print_tile t4;
  print_string "    ";
  print_rd rd5 Straight;
  print_string "\n"

let line_10 rd1 rd2 rd3 rd4 rd5 rd6 rd7 rd8 rd9 rd10  d1 d2=
  print_string "        ";
  print_rd rd1 d1;
  print_string "       ";
  print_rd rd2 d2;
  print_string "     ";
  print_rd rd3 d1;
  print_string "       ";
  print_rd rd4 d2;
  print_string "     ";
  print_rd rd5 d1;
  print_string "       ";
  print_rd rd6 d2;
  print_string "     ";
  print_rd rd7 d1;
  print_string "       ";
  print_rd rd8 d2;
  print_string "     ";
  print_rd rd9 d1;
  print_string "       ";
  print_rd rd10 d2;
  print_string "\n"

let line_11
    corn22
    corn23
    corn24
    corn25
    corn26
    corn27
    c22
    c23
    c24
    c25
    c26
    c27 =
  print_string "   ";
  ignore (print_corner corn22 c22);
  print_string "         ";
  ignore (print_corner corn23 c23);
  print_string "         ";
  ignore (print_corner corn24 c24);
  print_string "         ";
  ignore (print_corner corn25 c25);
  print_string "         ";
  ignore (print_corner corn26 c26);
  print_string "         ";
  ignore (print_corner corn27 c27);
  print_string "\n"

let line_12 rd1 rd2 rd3 rd4 rd5 rd6 t1 t2 t3 t4 t5 =
  print_string "     ";
  print_rd rd1 Straight;
  print_string "   ";
  print_tile t1;
  print_string "    ";
  print_rd rd2 Straight;
  print_string "   ";
  print_tile t2;
  print_string "    ";
  print_rd rd3 Straight;
  print_string "    ";
  print_tile t3;
  print_string "   ";
  print_rd rd4 Straight;
  print_string "   ";
  print_tile t4;
  print_string "    ";
  print_rd rd5 Straight;
  print_string "   ";
  print_tile t5;
  print_string "    ";
  print_rd rd6 Straight;
  print_string "\n"

let print_board_1
    (corners : node array)
    (roads : road array array)
    (tiles : tile list) =
  line_1 corners.(0) corners.(1) corners.(2) 1 2 3;
  line_2
    roads.(0).(3)
    roads.(0).(4)
    roads.(1).(4)
    roads.(1).(5)
    roads.(2).(5)
    roads.(2).(6) Forward Backward;
  line_3 corners.(3) corners.(4) corners.(5) corners.(6) 4 5 6 7;
  line_4
    roads.(3).(7)
    roads.(4).(8)
    roads.(5).(9)
    roads.(6).(10)
    (List.nth tiles 0) (List.nth tiles 1) (List.nth tiles 2);
  line_3 corners.(7) corners.(8) corners.(9) corners.(10) 8 9 10 11;
  line_6
    roads.(7).(11)
    roads.(7).(12)
    roads.(8).(12)
    roads.(8).(13)
    roads.(9).(13)
    roads.(9).(14)
    roads.(10).(14)
    roads.(10).(15) Forward Backward;
  line_7 corners.(11) corners.(12) corners.(13) corners.(14)
    corners.(15) 12 13 14 15 16;
  line_8
    roads.(11).(16)
    roads.(12).(17)
    roads.(13).(18)
    roads.(14).(19)
    roads.(15).(20)
    (List.nth tiles 3) (List.nth tiles 4) (List.nth tiles 5)
    (List.nth tiles 6);
  line_7 corners.(16) corners.(17) corners.(18) corners.(19)
    corners.(20) 17 18 19 20 21;
  line_10
    roads.(16).(21)
    roads.(16).(22)
    roads.(17).(22)
    roads.(17).(23)
    roads.(18).(23)
    roads.(18).(24)
    roads.(19).(24)
    roads.(19).(25)
    roads.(20).(25)
    roads.(20).(26) Forward Backward;
  line_11 corners.(21) corners.(22) corners.(23) corners.(24)
    corners.(25) corners.(26) 22 23 24 25 26 27;
  line_12
    roads.(21).(27)
    roads.(22).(28)
    roads.(23).(29)
    roads.(24).(30)
    roads.(25).(31)
    roads.(26).(32)
    (List.nth tiles 7) (List.nth tiles 8) (List.nth tiles 9)
    (List.nth tiles 10) (List.nth tiles 11);
  line_11 corners.(27) corners.(28) corners.(29) corners.(30)
    corners.(31) corners.(32) 28 29 30 31 32 33;
  line_10
    roads.(27).(33)
    roads.(28).(33)
    roads.(28).(34)
    roads.(29).(34)
    roads.(29).(35)
    roads.(30).(35)
    roads.(30).(36)
    roads.(31).(36)
    roads.(31).(37)
    roads.(32).(37) Backward Forward;
  line_7 corners.(33) corners.(34) corners.(35) corners.(36)
    corners.(37) 34 35 36 37 38;
  line_8
    roads.(32).(38)
    roads.(33).(39)
    roads.(35).(40)
    roads.(36).(41)
    roads.(37).(42)
    (List.nth tiles 12) (List.nth tiles 13) (List.nth tiles 14)
    (List.nth tiles 15);
  line_7 corners.(38) corners.(39) corners.(40) corners.(41)
    corners.(32) 39 40 41 42 43;
  line_6
    roads.(38).(43)
    roads.(39).(43)
    roads.(39).(44)
    roads.(40).(44)
    roads.(40).(45)
    roads.(41).(45)
    roads.(41).(46)
    roads.(42).(46) Backward Forward;
  line_3 corners.(43) corners.(44) corners.(45) corners.(46) 44 45 46 47;
  line_4
    roads.(43).(47)
    roads.(44).(48)
    roads.(45).(49)
    roads.(46).(50)
    (List.nth tiles 16) (List.nth tiles 17) (List.nth tiles 18);
  line_3 corners.(47) corners.(48) corners.(49) corners.(50) 48 49 50 51;
  line_2
    roads.(47).(51)
    roads.(48).(51)
    roads.(48).(52)
    roads.(49).(52)
    roads.(49).(53)
    roads.(50).(53) Backward Forward;
  line_1 corners.(51) corners.(52) corners.(53) 52 53 54;
  print_string "\n\n" *)

let print_corner_row corners  start size = 
  print_string "\n   ";
  let rec print_init_space x = (
    match (x < 6) with
    | true -> (
      print_string "       ";
      print_init_space (x+1)
    )
    | false -> () )
  in print_init_space (size);
  for i = 0 to size-1 do 
    print_corner corners.(start + i) (start + i);
    print_string "         ";
  done
 

let print_road_row roads n start size = (* size is 3 4 5 *)
  print_string "\n        ";
  let rec print_init_space x = (
    match size with
    | 4 -> print_string "       "
    | 3 -> print_string "              "
    | _ -> ())
  in print_init_space (size);
  for i = 0 to size-1 do 
    if n < 12 then (* top half *) begin
      print_rd roads.(start+i).(start+i+size) Forward;
      print_string "       ";
      print_rd roads.(start+i).(start+i+size+1) Backward;
      print_string "     "; end
    else  begin
      print_rd roads.(start+i).(start+i-size-1) Backward;
      print_string "       ";
      print_rd roads.(start+i).(start+i-size) Forward;
      print_string "     ";end 
  done
  

let print_tile_row roads tiles  size t_start rd_start= 
  print_string "\n     ";
  let rec print_init_space x = (
    match (x < 5) with
    | true -> (
      print_string "       ";
      print_init_space (x+1)
    )
    | false -> () )
  in print_init_space (size);
  (for i = 0 to size-1 do 
    print_rd roads.(rd_start+i).(rd_start+i+size+1) Straight;
    print_string "   ";
    print_tile (List.nth tiles (t_start+i));
    print_string "    "
  done);
  print_rd roads.(rd_start+size).(rd_start+size+1) Straight
  


let print_board (corners : node array)
(roads : road array array)
(tiles : tile list) = 
  print_corner_row corners  1 3;
  print_road_row roads 2 1 3 ;
  print_corner_row corners  4 4;
  print_tile_row roads tiles 3 0 4;
  print_corner_row corners  8 4;
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
  print_tile_row roads tiles 3 16 42;
  print_corner_row corners 48 4;
  print_road_row roads 22 52 3;
  print_corner_row corners 52 3;
  print_string "\n\n"
