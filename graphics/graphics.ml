open Raylib
open Game

let boxWidth : int = 80

let rec drawGrid m =
  for i = 0 to Array.length m - 1 do
    for j = 0 to Array.length (Array.get m i) - 1 do
      if Board.ismine m i j = -1 then
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

let setup () =
  init_window 1000 900 "Minesweeper";
  set_mouse_scale 1. 1.

let thisgameboard = Board.newboard 10 12 40

let rec loop () =
  if Raylib.window_should_close () then Raylib.close_window ()
  else
    let open Raylib in
    begin_drawing ();
    clear_background Color.raywhite;
    draw_text "minesweeper? i hardly know her!" 350 450 20 Color.black;
    if is_cursor_on_screen () then (
      clear_background Color.raywhite;
      drawGrid (Board.boardwithvalue thisgameboard));
    end_drawing ();
    loop ()

let () = setup () |> loop
