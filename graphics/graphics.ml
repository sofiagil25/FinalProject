open Raylib
open Game

let boxWidth : int = 80
let screenwidth = 1200

let intro_left_margin = screenwidth/6
let left_margin = screenwidth/6

type info_packet = { textures : Texture2D.t list }

let setup () =
  init_window screenwidth screenwidth "Minesweeper";
  set_target_fps 60;
  set_mouse_scale 1. 1.;

  let micheal_clarkson = load_image "img/clarkson.png" in
  let coin = load_image "img/coin.png" in
  let heart = load_image "img/heart.png" in
  let bomb = load_image "img/bomb.png" in
  let micheal_clarkson_mad = load_image "img/clarkson.jpeg" in
  let dexter_kozen = load_image "img/dexter.png" in
  let derek = load_image "img/derek.png" in
  let sofia = load_image "img/sofia.png"in
  let lauren = load_image "img/lauren.png"in


  image_resize (addr micheal_clarkson) 200 200;
  (*used*)
  image_resize (addr coin) 200 200;
  image_resize (addr heart) 200 200;
  image_resize (addr bomb) boxWidth boxWidth;
  image_resize (addr micheal_clarkson_mad) 200 200;
  (*used*)
  image_resize (addr dexter_kozen) 200 200;
  image_resize (addr derek) 200 200;
  image_resize (addr sofia) 200 200;
  image_resize (addr lauren) 200 200;


  let clarkson_texture = load_texture_from_image micheal_clarkson in
  let coin_texture = load_texture_from_image coin in
  let heart_texture = load_texture_from_image heart in
  let bomb_texture = load_texture_from_image bomb in
  let clarkson_mad_texture = load_texture_from_image micheal_clarkson_mad in
  let dexter_texture = load_texture_from_image dexter_kozen in
  let derek_texture = load_texture_from_image derek in
  let sofia_texture = load_texture_from_image sofia in
  let lauren_texture = load_texture_from_image lauren in



  unload_image micheal_clarkson;
  unload_image coin;
  unload_image heart;
  unload_image bomb;
  unload_image micheal_clarkson_mad;
  unload_image dexter_kozen;
  unload_image derek;
  unload_image sofia;
  unload_image lauren;
  {
    textures =
      [
        clarkson_texture;
        coin_texture;
        heart_texture;
        bomb_texture;
        clarkson_mad_texture;
        dexter_texture;
        derek_texture;
        sofia_texture;
        lauren_texture;
      ];
  }

let currx = ref 6
let curry = ref 6
let flagstate = ref false

let edit_game = ref false
let start_mode = ref true
let currprob = ref 25
let currtotaltime = ref 0.
let currstarttime = ref 0.
let left_margin = screenwidth/6

let thisgameboard = ref (Board.newboard !currx !curry !currprob)
let started = ref false
let top_bar_size = (screenwidth / 2) - (boxWidth / 2 * !currx)-75

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

let rec drawGrid m textures=
  for i = 0 to Array.length m - 1 do
    for j = 0 to Array.length (Array.get m i) - 1 do
      if Board.getflag m i j = false then(
        if
          match rects.(i).(j) with
          | _, bool -> not bool
        then
          draw_rectangle (x_coordinate boxWidth i) (y_coordinate boxWidth j)
            boxWidth boxWidth Color.black
        else if( Board.ismine m i j = -1 && 
          Board.getflag m i j = false&& !flagstate=false) then
          draw_rectangle (x_coordinate boxWidth i) (y_coordinate boxWidth j)
            boxWidth boxWidth Color.red
        else if (!flagstate=true) then
        (
            draw_texture (List.nth textures 3) (x_coordinate boxWidth i)
            (y_coordinate boxWidth j) Color.raywhite
        )
        else (
          draw_rectangle (x_coordinate boxWidth i) (y_coordinate boxWidth j)
            boxWidth boxWidth Color.gray;
          draw_text
            (string_of_int (Board.getcount m i j))
            (x_coordinate boxWidth i + (boxWidth / 2))
            (y_coordinate boxWidth j + (boxWidth / 2))
            20 Color.black))
      else if Board.getflag m i j = true then(
        (print_string "GET GET GET GET flag = true";
        draw_rectangle (x_coordinate boxWidth i) (y_coordinate boxWidth j)
          boxWidth boxWidth Color.yellow;
        draw_texture (List.nth textures 3) (x_coordinate boxWidth i)
         (y_coordinate boxWidth j) Color.raywhite))

        
    done
  done

let alive = ref true
let isZero x y = Board.getcount (Board.boardwithvalue !thisgameboard) x y = 0
let isBomb x y = Board.ismine (Board.boardwithvalue !thisgameboard) x y = -1

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

let flag_button =
  Rectangle.create
    (750|>float_of_int)
    (230|>float_of_int)
    100. 50.

let draw_flag_button () =
  draw_rectangle
    (750)
    (230)
    100 50 Color.yellow;
  draw_text "FLAG"
    (760)
    (250)
    30 Color.red

let eval_flagstate m =
  draw_flag_button ();
  if is_mouse_button_pressed MouseButton.Left then
    if check_collision_point_rec (get_mouse_position ()) flag_button then
      if !flagstate = false then (
        flagstate := true;
        for i = 0 to Array.length m - 1 do
          for j = 0 to Array.length (Array.get m i) - 1 do
            Board.setflag m i j
          done
        done)
      else if !flagstate = true then flagstate := false

let draw_easy_button () =
  draw_rectangle
    (intro_left_margin)
    (* ((screenwidth / 4) - 300) *)
    (screenwidth / 10) 200 100 Color.black;
  draw_text "Easy"
    (intro_left_margin+60)
    ((screenwidth / 10) + 20)
    30 Color.red;
  draw_text "Start"
    (intro_left_margin+60)
    ((screenwidth / 10) + 50)
    30 Color.red

let draw_medium_button () =
  draw_rectangle (left_margin  + 300) (screenwidth / 10) 200 100 Color.black;
  draw_text "Medium"
    (left_margin  + 360 )
    ((screenwidth / 10) + 20)
    30 Color.red;
  draw_text "Start"
    (left_margin  + 360)
    ((screenwidth / 10) + 50)
    30 Color.red

let draw_hard_button () =
  draw_rectangle
    (left_margin  + 600)
    (screenwidth / 10) 200 100 Color.black;
  draw_text "Hard"
    (left_margin + 660)
    ((screenwidth / 10) + 20)
    30 Color.red;
  draw_text "Start"
    (left_margin + 660)
    ((screenwidth / 10) + 50)
    30 Color.red

let draw_info_button () =
  draw_rectangle
    ((screenwidth / 2) - 350)
    ((screenwidth / 10) + 500)
    200 100 Color.blue;
  draw_text "Learn"
    ((screenwidth / 2) - 290)
    ((screenwidth / 10) + 520)
    30 Color.white;
  draw_text "More"
    ((screenwidth / 2) - 290)
    ((screenwidth / 10) + 550)
    30 Color.white

let draw_team_button () =
  draw_rectangle
    (intro_left_margin+400)
    ((screenwidth / 10) + 500)
    200 100 Color.orange;
  draw_text "Meet"
    (intro_left_margin+460)
    ((screenwidth / 10) + 520)
    30 Color.black;
  draw_text "Team"
    (intro_left_margin+460)
    ((screenwidth / 10) + 550)
    30 Color.black

let draw_exit_button () =
  draw_rectangle
    ((screenwidth / 2) - 500)
    ((screenwidth / 10) - 100)
    200 100 Color.red;
  draw_text "Exit"
    ((screenwidth / 2) - 470)
    ((screenwidth / 10) - 80)
    30 Color.white;
  draw_text "To Menu"
    ((screenwidth / 2) - 470)
    ((screenwidth / 10) - 50)
    30 Color.white

let easy_button =
  Rectangle.create
    (intro_left_margin|>float_of_int)
    (float_of_int (screenwidth / 10))
    200. 100.

let medium_button =
  Rectangle.create
  ((intro_left_margin|>float_of_int)+.360.)
  (float_of_int (screenwidth / 10))
    200. 100.

let hard_button =
  Rectangle.create
  ((intro_left_margin|>float_of_int)+.600.)
    (float_of_int (screenwidth / 10))
    200. 100.

let info_button =
  Rectangle.create
    ((intro_left_margin|>float_of_int)+.100.)
    (float_of_int ((screenwidth / 10) + 500))
    200. 100.

let team_button =
  Rectangle.create
    ((intro_left_margin|>float_of_int)+.400.)
    (((screenwidth / 10) + 550)|>float_of_int)
    200. 100.

let exit_button =
  Rectangle.create
    (float_of_int ((screenwidth / 2) - 500))
    (float_of_int ((screenwidth / 10) - 100))
    200. 100.

let easystate = ref false
let mediumstate = ref false
let hardstate = ref false
let info = ref false
let team = ref false
let menu = ref true

let game_start () =
  started := true;
  currstarttime := Raylib.get_time ();
  currtotaltime := 30.

let easy_mode () =
  currtotaltime := 150.;
  currprob := 10;
  thisgameboard := Board.newboard !currx !curry !currprob;
  game_start ()

let medium_mode () =
  currtotaltime := 150.;
  currprob := 25;
  thisgameboard := Board.newboard !currx !curry !currprob;
  currtotaltime := 60.;
  game_start ()

let hard_mode () =
  currtotaltime := 150.;
  currprob := 35;
  thisgameboard := Board.newboard !currx !curry !currprob;
  (* this is what breaks the cide *)
  currtotaltime := 40.;
  game_start ()

let eval_state () =
  if check_collision_point_rec (get_mouse_position ()) easy_button then (
    easystate := true;
    edit_game := true;
    start_mode := false;
    menu := false;
    team := false;
    info := false;
    easy_mode ())
  else if check_collision_point_rec (get_mouse_position ()) medium_button then (
    mediumstate := true;
    edit_game := true;
    start_mode := false;
    menu := false;
    team := false;
    info := false;
    medium_mode ())
  else if check_collision_point_rec (get_mouse_position ()) hard_button then (
    hardstate := true;
    edit_game := true;
    start_mode := false;
    menu := false;
    team := false;
    info := false;
    game_start ())
  else if check_collision_point_rec (get_mouse_position ()) info_button then (
    menu := false;
    team := false;
    info := true)
  else if check_collision_point_rec (get_mouse_position ()) team_button then (
    menu := false;
    team := true;
    info := false)

let eval_exit () =
  if check_collision_point_rec (get_mouse_position ()) exit_button then
    menu := true

let buttonn =
  Rectangle.create
    (float_of_int ((Array.length !thisgameboard * boxWidth / 2) + 50))
    (float_of_int (Array.length (Array.get !thisgameboard 0) * boxWidth / 2))
    200. 100.

let draw_lose textures =
  draw_text "you are a loser :c"
    ((Array.length !thisgameboard * boxWidth / 3) - 50)
    (Array.length (Array.get !thisgameboard 0) * boxWidth / 3- 100)
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
    ((Array.length (Array.get !thisgameboard 0) * boxWidth / 3) - 40)
    40 Color.orange;
  draw_text "ye"
    (Array.length !thisgameboard * boxWidth / 3)
    ((Array.length (Array.get !thisgameboard 0) * boxWidth / 2) + 25)
    50 Color.black;
  draw_text "nah"
    ((Array.length !thisgameboard * boxWidth / 2) + 110)
    ((Array.length (Array.get !thisgameboard 0) * boxWidth / 2) + 25)
    50 Color.black;
  draw_texture (List.nth textures 6) 500 200 Color.raywhite

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

let restart_game textures =
  clear_background Color.white;
  (* thisgameboard := Board.newboard !currx !curry !currprob; *)
  flagstate := false;
  resetBoard 0 0;
  started := true;
  alive := true;
  currstarttime := Raylib.get_time ();
  if !easystate = true then (
    currtotaltime := 100.;
    currprob := 20;
    thisgameboard := Board.newboard !currx !curry !currprob)
  else if !mediumstate = true then (
    currtotaltime := 45.;
    currprob := 40;
    thisgameboard := Board.newboard !currx !curry !currprob)
  else (
    currtotaltime := 31.;
    currprob := 40;
    thisgameboard := Board.newboard !currx !curry !currprob);
  drawGrid (Board.boardwithvalue !thisgameboard) textures

let quit_game () =
  end_drawing ();
  started := true;
  close := true

let draw_win textures =
  clear_background Color.raywhite;
  draw_rectangle top_bar_size top_bar_size 200 100 Color.pink;
  draw_rectangle (top_bar_size + 210) top_bar_size 200 100 Color.pink;
  draw_text "YIPEEEEEEE, YOU WON" (top_bar_size - 100) (top_bar_size - 200) 50
    Color.pink;
  draw_text "another challenge?" (top_bar_size - 190) (top_bar_size - 150) 25
    Color.pink;
  draw_texture (List.nth textures 0) left_margin 200 Color.raywhite;
  draw_text "ye" (top_bar_size + 10) top_bar_size 50 Color.black;
  draw_texture (List.nth textures 5) 500 200 Color.raywhite;
  draw_text "nah" (top_bar_size + 220) top_bar_size 50 Color.black

let buttonywin =
  Rectangle.create
    (top_bar_size |> float_of_int)
    (top_bar_size |> float_of_int)
    200. 100.

let buttonnwin =
  Rectangle.create
    (top_bar_size + 210 |> float_of_int)
    (top_bar_size |> float_of_int)
    200. 100.

let ifwin = ref false

let draw_instructions () =
  clear_background Color.green;
  draw_text "Press a start button to start playing" ((screenwidth / 2) - 300) 
  ((screenwidth / 10) + 270) 30 Color.blue;
  draw_text "Instructions: The numbers in the gray boxes " (intro_left_margin)
    ((screenwidth / 10) + 120)
    30 Color.black;
  draw_text "correspond to the amount of mines directly touching " 
  (intro_left_margin)
    ((screenwidth / 10) + 155)
    30 Color.black;
  draw_text "that box (adjacent). If the timer runs out, "
   (intro_left_margin)
    ((screenwidth / 10) + 190)
    30 Color.black;
  draw_text "or you click a bomb, you lose!"
  (intro_left_margin)
  ((screenwidth/10) + 225)
  30 Color.black

let draw_beginning_screen () =
  draw_instructions ();
  draw_easy_button ();
  draw_medium_button ();
  draw_hard_button ();
  draw_info_button ();
  draw_team_button ()

let draw_team textures =
  clear_background Color.beige;
  draw_exit_button ();
  draw_texture (List.nth textures 7) 70 150 Color.raywhite;
  draw_text "Sofia is a computer science major from New York State." 300 150 25
    Color.black;
  draw_text "She LOVES mindsweeper, reading, and listening to sad" 300 175 25
    Color.black;
  draw_text "music. She also enjoys doing cryptic crosswords! She" 300 200 25
    Color.black;
  draw_text "is pictured here holding a small bird." 300 225 25 Color.black;
  draw_text "" 300 250 25 Color.black;
  draw_text "" 300 275 25 Color.black;
  draw_texture (List.nth textures 8) 600 300 Color.raywhite;
  draw_text "Hi! My name is Lauren Lee and i’m a sophomore in biomedical" 300
    300 25 Color.black;
  draw_text "engineering. I just really love OCaml probably more than " 300 325
    25 Color.black;
  draw_text "anything so I decided to take this class! Go big red!" 300 350 25
    Color.black;
  draw_text "" 300 375 25 Color.black;
  draw_text "" 300 400 25 Color.black;
  draw_texture (List.nth textures 6) 600 425 Color.raywhite;
  draw_text "Hey I'm Derek, a freshman doing computer science. I love" 300 425
    25 Color.black;
  draw_text "eating cheesecake and doing OCaml in my free time! I'm a" 300 450
    25 Color.black;
  draw_text "big Florida Panthers fan!" 300 475 25 Color.black;
  draw_text "" 300 500 25 Color.black;
  draw_text "" 300 520 25 Color.black;
  draw_text "Aneesha kodati is a sophomore studying computer science" 300 550 25
    Color.black;
  draw_text "in the College of Engineering. When she’s not coding in" 300 575 25
    Color.black;
  draw_text "OCAML, she likes to dance or code more for CUAIR!" 300 600 25
    Color.black

let draw_info () : unit =
  clear_background Color.beige;
  draw_exit_button ();
  draw_text
    "Minesweeper is a classic puzzle video game that has been popularized on \
     various computer platforms."
    100 120 25 Color.black;
  draw_text
    "The objective of the game is to clear a rectangular grid containing \
     hidden mines without detonating any of them."
    100 150 25 Color.black;
  draw_text
    "The player must strategically uncover squares on the grid, with each \
     square either revealing a number indicating the number of adjacent mines \
     or being empty."
    100 175 25 Color.black;
  draw_text
    "By using the revealed numbers as clues, the player must deduce the \
     locations of the mines and mark them with flags."
    100 200 25 Color.black;
  draw_text "" 100 200 25 Color.black;
  draw_text
    "The history of Minesweeper dates back to the 1960s when it originated as \
     a mainframe computer game called \"Cube.\""
    100 275 25 Color.black;
  draw_text
    "However, the version we are most familiar with today was developed for \
     the Microsoft Windows operating system."
    100 300 25 Color.black;
  draw_text
    "Minesweeper was introduced as a part of the Windows Entertainment Pack in \
     1990, bundled with other simple games."
    100 325 25 Color.black;
  draw_text "" 100 300 25 Color.black;
  draw_text
    "The popularity of Minesweeper soared with the release of Windows 3.1 in \
     1992, as it was included as a standard pre-installed game."
    100 375 25 Color.black;
  draw_text
    "It quickly became a beloved pastime for many Windows users, offering a \
     challenging and addictive gameplay experience."
    100 400 25 Color.black;
  draw_text "" 100 375 25 Color.black;
  draw_text
    "The mechanics and rules of Minesweeper are relatively straightforward, \
     making it accessible to players of all ages."
    100 425 25 Color.black;
  draw_text
    "Its popularity grew further due to its inclusion in subsequent Windows \
     versions, ensuring its availability to a wide audience."
    100 450 25 Color.black;
  draw_text
    "The game's simplicity, coupled with its puzzle-solving aspect, has made \
     it a favorite time-killer for many."
    100 475 25 Color.black;
  draw_text "" 100 475 25 Color.black;
  draw_text "Minesweeper's influence also extended beyond the Windows platform."
    100 525 25 Color.black;
  draw_text
    "It was adapted for various other operating systems, including macOS, \
     Linux, and mobile platforms."
    100 550 25 Color.black;
  draw_text
    "Numerous clones and variants of the game were developed for different \
     devices and gaming platforms, contributing to its enduring appeal."
    100 575 25 Color.black;
  draw_text "" 100 575 25 Color.black;
  draw_text
    "While Minesweeper might not boast the graphical sophistication or \
     complexity of modern games,"
    100 625 25 Color.black;
  draw_text
    "its strategic gameplay and addictive nature have allowed it to stand the \
     test of time."
    100 650 25 Color.black;
  draw_text
    "It remains a beloved and iconic game in the history of computer gaming, \
     recognized for its simplicity, logical thinking requirements, and"
    100 675 25 Color.black

let lose textures =
  drawGrid (Board.boardwithvalue !thisgameboard) textures;
  clear_background Color.raywhite;
  draw_lose textures

let halftime () =
  let intoftime = !currtotaltime |> int_of_float in
  intoftime / 2

let draw_time orig_seconds remaining_seconds =
  let ycoord = top_bar_size - 100 in
  let offset = 30 in
  let xcoord = top_bar_size + offset in
  if !currtotaltime = 30. && !alive = true then (
    match remaining_seconds with
    | _ ->
        draw_text "Practice ends in " xcoord (ycoord - 50) 40 Color.red;
        draw_text
          (string_of_int remaining_seconds)
          (xcoord + 100) ycoord 50 Color.red)
  else if !easystate = true && !alive = true then (
    draw_text "Time remaining: " (xcoord - offset) (ycoord - 75) 50 Color.red;
    match remaining_seconds with
    | 75 ->
        draw_text "25% there. you got this." (xcoord + offset) ycoord 30
          Color.red
    | 50 -> draw_text "HALF TIMEEEEEE" (xcoord + offset) ycoord 50 Color.red
    | 20 -> draw_text "20 SECS LEFT" (xcoord + offset) ycoord 50 Color.red
    | 10 -> draw_text "10 SECS LEFT!!!!" (xcoord + offset) ycoord 50 Color.red
    | 5 -> draw_text "FIVE SECONDS!!" (xcoord + offset) ycoord 50 Color.red
    | 4 -> draw_text "HURRY UP BRUH" (xcoord + offset) ycoord 50 Color.red
    | 3 -> draw_text "THREE SECS LEFT" (xcoord + offset) ycoord 50 Color.red
    | 2 -> draw_text "TWOOOOOOO" (xcoord + offset) ycoord 50 Color.red
    | 1 -> draw_text "ONEEEEEEE" (xcoord + offset) ycoord 50 Color.red
    | 0 -> draw_text "YOU LOSEEEEEE" (xcoord + 70) ycoord 50 Color.red
    | _ ->
        draw_text
          (string_of_int remaining_seconds)
          (xcoord + 70) ycoord 50 Color.red)
  else if !mediumstate = true && !alive = true then (
    draw_text "Time remaining: " (xcoord - offset) (ycoord - 75) 50 Color.red;

    match remaining_seconds with
    | 22 -> draw_text "HALF TIMEEE" (xcoord + offset) ycoord 50 Color.red
    | 10 ->
        draw_text "75% there. you got this." (xcoord + offset) ycoord 30
          Color.red
    | 5 -> draw_text "FIVE SECONDS!!" (xcoord + offset) ycoord 50 Color.red
    | 4 -> draw_text "HURRY UP BRUH" (xcoord + offset) ycoord 50 Color.red
    | 3 -> draw_text "THREE SECS LEFT" (xcoord + offset) ycoord 50 Color.red
    | 2 -> draw_text "TWOOOOOOO" (xcoord + offset) ycoord 50 Color.red
    | 1 -> draw_text "ONEEEEEEE" (xcoord + offset) ycoord 50 Color.red
    | 0 -> draw_text "YOU LOSEEEEEE" (xcoord + 70) ycoord 50 Color.red
    | _ ->
        draw_text
          (string_of_int remaining_seconds)
          (xcoord + 70) ycoord 50 Color.red)
  else if !hardstate = true && !alive = true then (
    draw_text "Time remaining: " (xcoord - offset) (ycoord - 75) 50 Color.red;

    match remaining_seconds with
    | 15 -> draw_text "HALF TIMEEE" (xcoord + offset) ycoord 50 Color.red
    | 10 ->
        draw_text "10 seconds. you got this." (xcoord + offset) ycoord 30
          Color.red
    | 5 -> draw_text "FIVE SECONDS!!" (xcoord + offset) ycoord 50 Color.red
    | 4 -> draw_text "HURRY UP BRUH" (xcoord + offset) ycoord 50 Color.red
    | 3 -> draw_text "THREE SECS LEFT" (xcoord + offset) ycoord 50 Color.red
    | 2 -> draw_text "TWOOOOOOO" (xcoord + offset) ycoord 50 Color.red
    | 1 -> draw_text "ONEEEEEEE" (xcoord + offset) ycoord 50 Color.red
    | 0 ->
        draw_text "YOU LOSE. it's ok that was hard." (xcoord + 70) ycoord 20
          Color.red
    | _ ->
        draw_text
          (string_of_int remaining_seconds)
          (xcoord + 70) ycoord 50 Color.red)

let rec loop () info_packet =
  let textures=info_packet.textures in
  if Raylib.window_should_close () || !close = true then Raylib.close_window ()
  else
    let elapsed_time = Raylib.get_time () -. !currstarttime in
    let time_left = int_of_float !currtotaltime - int_of_float elapsed_time in
    begin_drawing ();
    if !start_mode = true && !menu = true then (
      if is_mouse_button_pressed MouseButton.Left then eval_state ())
    else if !start_mode = true && (!info = true || !team = true) then
      if is_mouse_button_pressed MouseButton.Left then eval_exit ();
    if !started = true && !ifwin = false && time_left >= 0 then (
      draw_stats ();
      eval_flagstate (Board.boardwithvalue !thisgameboard);
      draw_time
        ((elapsed_time |> int_of_float)
        + max (int_of_float !currtotaltime - int_of_float elapsed_time) 0)
        (max (int_of_float !currtotaltime - int_of_float elapsed_time) 0);
      if not !alive then (
        lose textures;
        if is_mouse_button_pressed MouseButton.Left then
          if check_collision_point_rec (get_mouse_position ()) buttony then
            restart_game textures
          else if check_collision_point_rec (get_mouse_position ()) buttonn then
            quit_game ())
      else if is_cursor_on_screen () then (
        if (!currx * !curry) - countuncovered () = countbombs () then (
          ifwin := true;
          draw_text "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" 50 50 100 Color.black)
        else clear_background Color.raywhite;
        drawGrid (Board.boardwithvalue !thisgameboard)textures;
        if is_mouse_button_pressed MouseButton.Left then
          let mouse_pos = get_mouse_position in
          findCollision (mouse_pos ())))
    else if !ifwin = false && !started = false && !menu = true then (
      draw_beginning_screen ();
      clear_background Color.white)
    else if !ifwin = false && !started = false && !info = true then (
      Raylib.clear_background Color.black;
      draw_info ();
      if is_mouse_button_pressed MouseButton.Left then eval_exit ())
    else if !ifwin = false && !started = false && !team = true then (
      Raylib.clear_background Color.black;
      draw_team textures;
      if is_mouse_button_pressed MouseButton.Left then eval_exit ())
    else if time_left < 0 then (
      lose textures;
      if is_mouse_button_pressed MouseButton.Left then
        if check_collision_point_rec (get_mouse_position ()) buttony then
          restart_game textures
        else if check_collision_point_rec (get_mouse_position ()) buttonn then
          quit_game ())
    else if time_left >= 0 then (
      draw_win textures;
      if is_mouse_button_pressed MouseButton.Left then
        if check_collision_point_rec (get_mouse_position ()) buttonywin then (
          ifwin := false;
          restart_game textures)
        else if check_collision_point_rec (get_mouse_position ()) buttonnwin
        then (
          ifwin := false;
          quit_game ()));

    end_drawing ();
    loop () { textures = info_packet.textures }

let () = setup () |> loop ()
