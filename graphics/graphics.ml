open Raylib
open Game

let boxWidth : int = 80

let setup () =
  init_window 1000 900 "Minesweeper";
  set_mouse_scale 1. 1.

let thisgameboard = ref (Board.newboard 5 5 15)

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
          (i * boxWidth |> float_of_int)
          (j * boxWidth |> float_of_int)
          (boxWidth |> float_of_int) (boxWidth |> float_of_int)
      in
      rects.(i).(j) <- (rect, false)
    done
  done;
  rects

let rects = buildRects !thisgameboard

let rec drawGrid m =
  for i = 0 to Array.length m - 1 do
    for j = 0 to Array.length (Array.get m i) - 1 do
      if
        match rects.(i).(j) with
        | _, bool -> not bool
      then
        draw_rectangle (i * boxWidth) (j * boxWidth) boxWidth boxWidth
          Color.black
      else if Board.ismine m i j = -1 then
        draw_rectangle (i * boxWidth) (j * boxWidth) boxWidth boxWidth Color.red
      else (
        draw_rectangle (i * boxWidth) (j * boxWidth) boxWidth boxWidth
          Color.gray;
        draw_text
          (string_of_int (Board.getcount m i j))
          ((i * boxWidth) + (boxWidth / 2))
          ((j * boxWidth) + (boxWidth / 2))
          20 Color.black)
    done
  done

let alive = ref true

let rec expandZeroTiles i j : unit =
  let isZero x y =
    Board.getcount (Board.boardwithvalue !thisgameboard) x y = 0
  in
  let isBomb x y =
    Board.ismine (Board.boardwithvalue !thisgameboard) x y = -1
  in
  let tileStatus x y =
    match rects.(x).(y) with
    | _, b -> b
  in
  let showTile x y =
    match rects.(x).(y) with
    | rectangle, _ -> rects.(x).(y) <- (rectangle, true)
  in
  if (not (isBomb i j)) && not (isZero i j) then begin
    (*if not a zero tile but also not a bomb tile, then reveal those around it*)
    showTile i j;
    if i - 1 >= 0 && (not (tileStatus (i - 1) j)) && not (isBomb (i - 1) j) then
      showTile (i - 1) j;
    if
      i + 1 < Array.length rects
      && (not (tileStatus (i + 1) j))
      && not (isBomb (i + 1) j)
    then showTile (i + 1) j;
    if j - 1 >= 0 && (not (tileStatus i (j - 1))) && not (isBomb i (j - 1)) then
      showTile i (j - 1);
    if
      j + 1 < Array.length rects.(0)
      && (not (tileStatus i (j + 1)))
      && not (isBomb i (j + 1))
    then showTile i (j + 1)
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
        if Board.ismine (Board.boardwithvalue !thisgameboard) i j = -1 then
          alive := false;
        if
          Board.getcount (Board.boardwithvalue !thisgameboard) i j = 0
          && Board.ismine (Board.boardwithvalue !thisgameboard) i j != -1
        then expandZeroTiles i j)
    done
  done

let buttony =
  Rectangle.create
    (float_of_int ((Array.length !thisgameboard * boxWidth / 3) - 70))
    (float_of_int (Array.length (Array.get !thisgameboard 0) * boxWidth / 2))
    200. 100.

let buttonn =
  Rectangle.create
    (float_of_int ((Array.length !thisgameboard * boxWidth / 2) + 50))
    (float_of_int (Array.length (Array.get !thisgameboard 0) * boxWidth / 2))
    200. 100.

let draw_lose () =
  draw_text "you are a loser :c"
    ((Array.length !thisgameboard * boxWidth / 3) - 50)
    (Array.length (Array.get !thisgameboard 0) * boxWidth / 3)
    50 Color.white;
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
    40 Color.white;
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
  if x < 12 then
    if y < 10 then (
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

let rec loop () =
  if Raylib.window_should_close () || !close = true then Raylib.close_window ()
  else
    let open Raylib in
    begin_drawing ();
    (* draw_text (string_of_int (countbombs ())) 350 450 50 Color.black;
       draw_text (string_of_int (countuncovered ())) 450 450 50 Color.black;
       draw_text (string_of_int (Array.length !thisgameboard * Array.length
       (Array.get !thisgameboard 0))) 550 450 50 Color.black; put these in a
       different place they show score*)
    if not !alive then (
      clear_background Color.raywhite;
      drawGrid (Board.boardwithvalue !thisgameboard);
      draw_lose ();
      if is_mouse_button_pressed MouseButton.Left then
        if check_collision_point_rec (get_mouse_position ()) buttony then (
          clear_background Color.white;
          thisgameboard := Board.newboard 10 12 15;
          resetBoard 0 0;
          alive := true;
          drawGrid (Board.boardwithvalue !thisgameboard))
        else if check_collision_point_rec (get_mouse_position ()) buttonn then (
          end_drawing ();
          close := true))
    else if is_cursor_on_screen () then (
      if
        (Array.length !thisgameboard * Array.length (Array.get !thisgameboard 0))
        - countuncovered ()
        = countbombs ()
      then (
        clear_background Color.raywhite;
        drawGrid (Board.boardwithvalue !thisgameboard);
        draw_text "yipeeee" 350 450 50 Color.black)
      else clear_background Color.raywhite;
      drawGrid (Board.boardwithvalue !thisgameboard);
      if is_mouse_button_pressed MouseButton.Left then
        let mouse_pos = get_mouse_position in
        findCollision (mouse_pos ()));
    end_drawing ();
    loop ()

let () = setup () |> loop
