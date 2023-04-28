open Raylib
open Game

let boxWidth : int = 80

let setup () =
  init_window 1000 900 "Minesweeper";
  set_mouse_scale 1. 1.

let thisgameboard = Board.newboard 10 12 15

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

let rects = buildRects thisgameboard

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
          12 Color.black)
    done
  done

let alive = ref true

let rec expandZeroTiles i j : unit =
  let isZero x y =
    Board.getcount (Board.boardwithvalue thisgameboard) x y = 0
  in
  let isBomb x y = Board.ismine (Board.boardwithvalue thisgameboard) x y = -1 in
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
        if Board.ismine (Board.boardwithvalue thisgameboard) i j = -1 then
          alive := false;
        if
          Board.getcount (Board.boardwithvalue thisgameboard) i j = 0
          && Board.ismine (Board.boardwithvalue thisgameboard) i j != -1
        then expandZeroTiles i j)
    done
  done

let rec loop () =
  if Raylib.window_should_close () then Raylib.close_window ()
  else
    let open Raylib in
    begin_drawing ();
    if not !alive then (
      clear_background Color.raywhite;
      drawGrid (Board.boardwithvalue thisgameboard);
      draw_text "you fucking lost" 350 450 50 Color.white)
    else if is_cursor_on_screen () then (
      draw_text "i fucking lost" 350 450 50 Color.white;
      clear_background Color.raywhite;
      drawGrid (Board.boardwithvalue thisgameboard);
      if is_mouse_button_pressed MouseButton.Left then
        let mouse_pos = get_mouse_position in
        findCollision (mouse_pos ()));
    end_drawing ();
    loop ()

let () = setup () |> loop
