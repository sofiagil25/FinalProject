open Raylib
open Game

let boxWidth : int = 80

let setup () =
  init_window 1000 900 "Minesweeper";
  set_mouse_scale 1. 1.

let thisgameboard = Board.newboard 10 12 40

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
        | _, bool -> bool
      then
        draw_rectangle (i * boxWidth) (j * boxWidth) boxWidth boxWidth
          Color.green
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

let findCollision mouse_pos =
  let rows = Array.length rects in
  let cols = Array.length rects.(0) in
  let final_rect = ref None in
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      let rectangle =
        match rects.(i).(j) with
        | rectangle, _ -> rectangle
      in
      if check_collision_point_rec mouse_pos rectangle then
        rects.(i).(j) <- (rectangle, true);
      final_rect := Some (rectangle, true)
    done
  done

let rec loop () =
  if Raylib.window_should_close () then Raylib.close_window ()
  else
    let open Raylib in
    begin_drawing ();
    clear_background Color.raywhite;
    draw_text "minesweeper? i hardly know her!" 350 450 20 Color.black;
    if is_cursor_on_screen () then (
      clear_background Color.raywhite;
      drawGrid (Board.boardwithvalue thisgameboard);
      if is_mouse_button_pressed MouseButton.Left then
        let mouse_pos = get_mouse_position in
        findCollision (mouse_pos ()));
    end_drawing ();
    loop ()

let () = setup () |> loop
