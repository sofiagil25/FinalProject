open Raylib
open Game

let boxWidth : int = 80
let screenwidth = 900

let setup () =
  init_window screenwidth screenwidth "Minesweeper";
  set_target_fps 60;
  set_mouse_scale 1. 1.

let currx = ref 4
let curry = ref 4
let edit_game = ref (false)

let start_mode=ref(true)
let currprob = ref 25
let currtotaltime = ref 0.
let currstarttime = ref 0.
let thisgameboard = ref (Board.newboard !currx !curry !currprob)
let started = ref false
let top_bar_size = (screenwidth / 2) - (boxWidth / 2 * !currx)


let rec buildRects m =
  let rects =
    Array.make_matrix (Array.length m)
      (Array.length m.(0))
      (Rectangle.create 0. 0. 0. 0., false)
  in
  for i = 0 to Array.length m - 1 do
    for j = 0 to Array.length (Array.get m i) - 1 do
      let rect =
        Rectangle.create
          (top_bar_size + (i * boxWidth) |> float_of_int)
          (top_bar_size + (j * boxWidth) |> float_of_int)
          (boxWidth |> float_of_int) (boxWidth |> float_of_int)
      in
      rects.(i).(j) <- (rect, false)
    done
  done;
  rects

let rects = buildRects !thisgameboard
let y_coordinate orig_width j = top_bar_size + (j * orig_width)
let x_coordinate orig_width x = top_bar_size + (x * orig_width)

let rec drawGrid m =
  for i = 0 to Array.length m - 1 do
    for j = 0 to Array.length (Array.get m i) - 1 do
      if Board.getflag m i j = false then
        if
          match rects.(i).(j) with
          | _, bool -> not bool
        then
          draw_rectangle (x_coordinate boxWidth i) (y_coordinate boxWidth j)
            boxWidth boxWidth Color.black
        else if Board.ismine m i j = -1 then
          draw_rectangle (x_coordinate boxWidth i) (y_coordinate boxWidth j)
            boxWidth boxWidth Color.red
        else (
          draw_rectangle (x_coordinate boxWidth i) (y_coordinate boxWidth j)
            boxWidth boxWidth Color.gray;
          draw_text
            (string_of_int (Board.getcount m i j))
            (x_coordinate boxWidth i + (boxWidth / 2))
            (y_coordinate boxWidth j + (boxWidth / 2))
            20 Color.black)
      else
        draw_rectangle (x_coordinate boxWidth i) (y_coordinate boxWidth j)
          boxWidth boxWidth Color.yellow
    done
  done

let alive = ref true
let isZero x y =
  Board.getcount (Board.boardwithvalue !thisgameboard) x y = 0



let isBomb x y =
    Board.ismine (Board.boardwithvalue !thisgameboard) x y = -1
    


let tileStatus x y =
 match rects.(x).(y) with
    | _, b -> b


let showTile x y =
 match rects.(x).(y) with
    | rectangle, _ -> rects.(x).(y) <- (rectangle, true)


let rec expandZeroTiles i j : unit =
  if (not (isBomb i j)) && not (isZero i j) then begin
    (*if not a zero tile but also not a bomb tile, then reveal those around it*)
    showTile i j;
    if i - 1 >= 0 && (not (tileStatus (i - 1) j)) && not (isBomb (i - 1) j) then
      if isZero (i - 1) j then expandZeroTiles (i - 1) j else showTile (i - 1) j;
    if
      i + 1 < Array.length rects
      && (not (tileStatus (i + 1) j))
      && not (isBomb (i + 1) j)
    then
      if isZero (i + 1) j then expandZeroTiles (i + 1) j else showTile (i + 1) j;
    if j - 1 >= 0 && (not (tileStatus i (j - 1))) && not (isBomb i (j - 1)) then
      if isZero i (j - 1) then expandZeroTiles i (j - 1) else showTile i (j - 1);
    if
      j + 1 < Array.length rects.(0)
      && (not (tileStatus i (j + 1)))
      && not (isBomb i (j + 1))
    then
      if isZero i (j + 1) then expandZeroTiles i (j + 1) else showTile i (j + 1)
  end
  else if isZero i j then begin
    (*if zero tile, then recurse on all those around it*)
    showTile i j;
    if i - 1 >= 0 && not (tileStatus (i - 1) j) then expandZeroTiles (i - 1) j;
    if i + 1 < Array.length rects && not (tileStatus (i + 1) j) then
      expandZeroTiles (i + 1) j;
    if j - 1 >= 0 && not (tileStatus i (j - 1)) then expandZeroTiles i (j - 1);
    if j + 1 < Array.length rects.(0) && not (tileStatus i (j + 1)) then
      expandZeroTiles i (j + 1)
  end

let flagstate = ref false

let findCollision mouse_pos =
  let rows = Array.length rects in
  let cols = Array.length rects.(0) in
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      let rectangle =
        match rects.(i).(j) with
        | rectangle, _ -> rectangle
      in
      if check_collision_point_rec mouse_pos rectangle then (
        rects.(i).(j) <- (rectangle, true);
        if !flagstate = false then (
          if Board.ismine (Board.boardwithvalue !thisgameboard) i j = -1 then
            alive := false;
          if
            Board.getcount (Board.boardwithvalue !thisgameboard) i j = 0
            && Board.ismine (Board.boardwithvalue !thisgameboard) i j != -1
          then expandZeroTiles i j)
        else Board.setflag (Board.boardwithvalue !thisgameboard) i j)
    done
  done

let buttony =
  Rectangle.create
    (float_of_int ((Array.length !thisgameboard * boxWidth / 3) - 70))
    (float_of_int (Array.length (Array.get !thisgameboard 0) * boxWidth / 2))
    200. 100.

let draw_easy_button () =
  draw_rectangle
    ((screenwidth / 2) - 300)
    (screenwidth / 10) 200 
    100 Color.black;
  draw_text "Easy"
    ((screenwidth / 2) - 240)
    ((screenwidth / 10) + 20)
    30 Color.red;
  draw_text "Start"
    ((screenwidth / 2) - 240)
    ((screenwidth / 10) + 50)
    30 Color.red

let draw_medium_button () =
  draw_rectangle (screenwidth / 2) (screenwidth / 10) 200 100 Color.black;
  draw_text "Medium"
    ((screenwidth / 2) + 50)
    ((screenwidth / 10) + 20)
    30 Color.red;
  draw_text "Start"
    ((screenwidth / 2) + 50)
    ((screenwidth / 10) + 50)
    30 Color.red

let draw_hard_button () =
  draw_rectangle
    ((screenwidth / 2) + 300)
    (screenwidth / 10) 200 100 Color.black;
  draw_text "Hard"
    ((screenwidth / 2) + 360)
    ((screenwidth / 10) + 20)
    30 Color.red;
  draw_text "Start"
    ((screenwidth / 2) + 360)
    ((screenwidth / 10) + 50)
    30 Color.red

let easy_button =
  Rectangle.create
    (float_of_int ((screenwidth / 2) - 300))
    (float_of_int (screenwidth / 10))
    200. 100.

let medium_button =
  Rectangle.create
    (float_of_int (screenwidth / 2))
    (float_of_int (screenwidth / 10))
    200. 100.

let hard_button =
  Rectangle.create
    (float_of_int ((screenwidth / 2) + 300))
    (float_of_int (screenwidth / 10))
    200. 100.

let easystate = ref false
let mediumstate = ref false
let hardstate = ref false

let game_start () =
  started := true;
  currstarttime := Raylib.get_time ();
  currtotaltime := 30.

let easy_mode()=
currtotaltime:= 150.;
currprob:=25;
thisgameboard:=Board.newboard !currx !curry !currprob;
game_start()

let medium_mode()= 
currtotaltime:= 150.;
currprob:=35;
thisgameboard:=Board.newboard !currx !curry !currprob;
currtotaltime:=60.;
game_start()

let hard_mode()= 
currtotaltime:= 150.;
currprob:=45;
thisgameboard:=Board.newboard !currx !curry !currprob; (* this is what breaks the cide *)
currtotaltime:=40.;
game_start()

let eval_state () =
  if check_collision_point_rec (get_mouse_position ()) easy_button then (
    easystate := true;
    edit_game:=true;
    start_mode:=false;
    easy_mode();
    )
  else if check_collision_point_rec (get_mouse_position ()) medium_button then (
    mediumstate := true;
    edit_game:=true;
    start_mode:=false;
    medium_mode();)
  else if check_collision_point_rec (get_mouse_position ()) hard_button then (
    hardstate := true;
    edit_game:=true;
    start_mode:=false;
    game_start ())

let buttonn =
  Rectangle.create
    (float_of_int ((Array.length !thisgameboard * boxWidth / 2) + 50))
    (float_of_int (Array.length (Array.get !thisgameboard 0) * boxWidth / 2))
    200. 100.

let draw_lose () =
  draw_text "you are a loser :c"
    ((Array.length !thisgameboard * boxWidth / 3) - 50)
    (Array.length (Array.get !thisgameboard 0) * boxWidth / 3)
    50 Color.orange;
  draw_rectangle
    ((Array.length !thisgameboard * boxWidth / 3) - 70)
    (Array.length (Array.get !thisgameboard 0) * boxWidth / 2)
    200 100 Color.white;
  draw_rectangle
    ((Array.length !thisgameboard * boxWidth / 2) + 50)
    (Array.length (Array.get !thisgameboard 0) * boxWidth / 2)
    200 100 Color.white;
  draw_text "try again?"
    ((Array.length !thisgameboard * boxWidth / 3) + 60)
    ((Array.length (Array.get !thisgameboard 0) * boxWidth / 3) + 70)
    40 Color.orange;
  draw_text "ye"
    (Array.length !thisgameboard * boxWidth / 3)
    ((Array.length (Array.get !thisgameboard 0) * boxWidth / 2) + 25)
    50 Color.black;
  draw_text "nah"
    ((Array.length !thisgameboard * boxWidth / 2) + 110)
    ((Array.length (Array.get !thisgameboard 0) * boxWidth / 2) + 25)
    50 Color.black

let close = ref false

let rec resetBoard (x : int) (y : int) =
  if x < !currx then
    if y < !curry then (
      match rects.(x).(y) with
      | rectangle, _ ->
          rects.(x).(y) <- (rectangle, false);
          resetBoard x (y + 1) (* else resetBoard x (y + 1); *))
    else resetBoard (x + 1) 0

let countbombs () : int =
  let x = ref 0 in
  for i = 0 to Array.length rects - 1 do
    for j = 0 to Array.length (Array.get rects i) - 1 do
      if Board.ismine (Board.boardwithvalue !thisgameboard) i j = -1 then
        x := !x + 1
      else if
        Board.getcount (Board.boardwithvalue !thisgameboard) i j = 0
        && Board.ismine (Board.boardwithvalue !thisgameboard) i j != -1
      then x := !x
    done
  done;
  !x

let countuncovered () : int =
  let x = ref 0 in
  for i = 0 to Array.length rects - 1 do
    for j = 0 to Array.length (Array.get rects i) - 1 do
      if
        match rects.(i).(j) with
        | _, bool -> not bool
      then x := !x
      else if Board.ismine !thisgameboard i j = -1 then x := !x
      else x := 1 + !x
    done
  done;
  !x

let draw_stats () =
  draw_text (string_of_int (countbombs ())) 350 450 50 Color.black;
  draw_text (string_of_int (countuncovered ())) 450 450 50 Color.black;
  draw_text
    (string_of_int
       (Array.length !thisgameboard * Array.length (Array.get !thisgameboard 0)))
    550 450 50 Color.black

let restart_game () =
  clear_background Color.white;
  (* thisgameboard := Board.newboard !currx !curry !currprob; *)
  flagstate := false;
  resetBoard 0 0;
  started := true;
  alive := true;
  currstarttime := Raylib.get_time ();
  if(!easystate=true) then (
    currtotaltime:=100.;
     currprob:=20; 
    thisgameboard:=Board.newboard !currx !curry !currprob;
  )
  else if !mediumstate=true then (
    currtotaltime:=45.;
     currprob:=40; 
    thisgameboard:=Board.newboard !currx !curry !currprob; 
  )
  else (
    currtotaltime:=31.;
    currprob:=40; 
    thisgameboard:=Board.newboard !currx !curry !currprob; );
  drawGrid (Board.boardwithvalue (!thisgameboard))

let quit_game () =
  end_drawing ();
  started := true;
  close := true

let draw_win () =
  clear_background Color.raywhite;
  draw_rectangle
  (top_bar_size) (top_bar_size) 200 100 Color.pink;
  draw_rectangle
  (top_bar_size+210) (top_bar_size) 200 100 Color.pink;
  draw_text "YIPEEEEEEE, YOU WON" (top_bar_size-100) 
  (top_bar_size-200) 50 Color.pink;
  draw_text "another challenge?"
  (top_bar_size-190) (top_bar_size-150) 25 Color.pink;
  draw_text "ye"
  (top_bar_size+10)(top_bar_size)
  50 Color.black;
  draw_text "nah"
  (top_bar_size+220)(top_bar_size)
  50 Color.black

let buttonywin =
    Rectangle.create
    ((top_bar_size)|>float_of_int) ((top_bar_size)|> float_of_int)200. 100.
  
let buttonnwin =
      Rectangle.create
      ((top_bar_size+210)|>float_of_int) ((top_bar_size)|> float_of_int)200. 100.
  
let ifwin = ref false

let draw_instructions () =
  clear_background Color.green;
  draw_text "Instructions: the thing does the thing" 50 50 30 Color.black;
  draw_text "Press any box to start playing" 350 650 30 Color.black

let draw_beginning_screen () =
  draw_instructions ();
  draw_easy_button ();
  draw_medium_button ();
  draw_hard_button ()
let lose () = 
  drawGrid (Board.boardwithvalue !thisgameboard);
  clear_background Color.raywhite;
  draw_lose ()
 
let halftime()=
  let intoftime =(!currtotaltime)|>int_of_float 
    in
  intoftime/2

let draw_time orig_seconds remaining_seconds =
  let ycoord = top_bar_size-50 in
  let offset=30 in
  let xcoord=top_bar_size+offset in 
  if (!currtotaltime=30. && !alive=true) then
    match remaining_seconds with 
    | _ -> draw_text "Practice ends in " (xcoord) (ycoord-50) 40 Color.red;
    draw_text (string_of_int remaining_seconds) (xcoord+100) ycoord 50 Color.red
  else if(!easystate=true && !alive=true) then(
    draw_text "Time remaining: " (xcoord-offset) (ycoord-75) (50) Color.red;
  match remaining_seconds with
  | 75 ->  draw_text "25% there. you got this." (xcoord+offset) ycoord 30 Color.red
  | 50 ->  draw_text "HALF TIMEEEEEE" (xcoord+offset) ycoord 50 Color.red
  | 20 -> draw_text "20 SECS LEFT" (xcoord+offset) ycoord 50 Color.red
  | 10 -> draw_text "10 SECS LEFT!!!!" (xcoord+offset) ycoord 50 Color.red
  | 5 -> draw_text "FIVE SECONDS!!" (xcoord+offset) ycoord 50 Color.red
  | 4 -> draw_text "HURRY UP BRUH" (xcoord+offset) ycoord 50 Color.red
  | 3 -> draw_text "THREE SECS LEFT" (xcoord+offset) ycoord 50 Color.red
  | 2 -> draw_text "TWOOOOOOO" (xcoord+offset) ycoord 50 Color.red
  | 1 -> draw_text "ONEEEEEEE" (xcoord+offset) ycoord 50 Color.red
  | 0 -> draw_text "YOU LOSEEEEEE" (xcoord+70) ycoord 50 Color.red
  | _ -> draw_text (string_of_int remaining_seconds) (xcoord+70) ycoord 50 Color.red)
  else if(!mediumstate=true && !alive=true) then(
    draw_text "Time remaining: " (xcoord-offset) (ycoord-75) (50) Color.red;

  match remaining_seconds with
  | 22 -> draw_text "HALF TIMEEE" (xcoord+offset) ycoord 50 Color.red
  | 10 -> draw_text "75% there. you got this." (xcoord+offset) ycoord 30 Color.red
  | 5 -> draw_text "FIVE SECONDS!!" (xcoord+offset) ycoord 50 Color.red
  | 4 -> draw_text "HURRY UP BRUH" (xcoord+offset) ycoord 50 Color.red
  | 3 -> draw_text "THREE SECS LEFT" (xcoord+offset) ycoord 50 Color.red
  | 2 -> draw_text "TWOOOOOOO" (xcoord+offset) ycoord 50 Color.red
  | 1 -> draw_text "ONEEEEEEE" (xcoord+offset) ycoord 50 Color.red
  | 0 -> draw_text "YOU LOSEEEEEE" (xcoord+70) ycoord 50 Color.red
  | _ -> draw_text (string_of_int remaining_seconds) (xcoord+70) ycoord 50 Color.red)
else if(!hardstate=true && !alive=true) then
  (draw_text "Time remaining: " (xcoord-offset) (ycoord-75) (50) Color.red;

  match remaining_seconds with
  | 15 -> draw_text "HALF TIMEEE" (xcoord+offset) ycoord 50 Color.red
  | 10 -> draw_text "10 seconds. you got this." (xcoord+offset) ycoord 30 Color.red
  | 5 -> draw_text "FIVE SECONDS!!" (xcoord+offset) ycoord 50 Color.red
  | 4 -> draw_text "HURRY UP BRUH" (xcoord+offset) ycoord 50 Color.red
  | 3 -> draw_text "THREE SECS LEFT" (xcoord+offset) ycoord 50 Color.red
  | 2 -> draw_text "TWOOOOOOO" (xcoord+offset) ycoord 50 Color.red
  | 1 -> draw_text "ONEEEEEEE" (xcoord+offset) ycoord 50 Color.red
  | 0 -> draw_text "YOU LOSE. it's ok that was hard." (xcoord+70) ycoord 20 Color.red
  | _ -> draw_text (string_of_int remaining_seconds) (xcoord+70) ycoord 50 Color.red)

let rec loop () =
  if Raylib.window_should_close () || !close = true then Raylib.close_window ()
  else
    let elapsed_time = Raylib.get_time () -. !currstarttime 
  in
    let time_left = int_of_float !currtotaltime - int_of_float elapsed_time 
  in
    begin_drawing ();
    if(!start_mode=true)then (
      if is_mouse_button_pressed MouseButton.Left then eval_state(); 
    );
    if !started = true && !ifwin = false && time_left >=0 then (
      draw_stats ();
      draw_time
       ((elapsed_time |> int_of_float) + 
       (max (int_of_float !currtotaltime - int_of_float elapsed_time) 0))
       (max (int_of_float !currtotaltime - int_of_float elapsed_time) 0);
      if not !alive then (
        lose();
        if is_mouse_button_pressed MouseButton.Left then
          if check_collision_point_rec (get_mouse_position ()) buttony then
            restart_game ()
          else if check_collision_point_rec (get_mouse_position ()) buttonn then
            quit_game ())
      else if is_cursor_on_screen () then (
        if (!currx * !curry) - countuncovered () = countbombs () then (
          ifwin := true;
          draw_text "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" 50 50 100 Color.black)
        else clear_background Color.raywhite;
        drawGrid (Board.boardwithvalue !thisgameboard);
        if is_mouse_button_pressed MouseButton.Left then
          let mouse_pos = get_mouse_position in
          findCollision (mouse_pos ())))
    else if !ifwin = false && !started=false then (
          draw_beginning_screen ();
          clear_background Color.white)
    else if time_left<0 then(
      lose();
      if is_mouse_button_pressed MouseButton.Left then
        if check_collision_point_rec (get_mouse_position ()) buttony then
          restart_game ()
        else if check_collision_point_rec (get_mouse_position ()) buttonn then
          quit_game ())
    else if time_left>=0 then (
      draw_win();
      if is_mouse_button_pressed MouseButton.Left then(
        if check_collision_point_rec (get_mouse_position ()) buttonywin then(
          ifwin:=false;
          restart_game ())
        else if check_collision_point_rec (get_mouse_position ()) buttonnwin then(
          ifwin:=false;
          quit_game ())
      ));
    

    end_drawing ();
    loop ()

let () = setup () |> loop
