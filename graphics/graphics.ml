open Raylib
open Game

let boxWidth : int = 80

let rec drawGrid m =
  for i = 0 to Array.length m - 1 do
    for j = 0 to Array.length (Array.get m i) - 1 do
      if Board.ismine m i j = -1 then
        draw_rectangle_lines (i * boxWidth) (j * boxWidth) boxWidth boxWidth
          Color.red
      else
        draw_rectangle_lines (i * boxWidth) (j * boxWidth) boxWidth boxWidth
          Color.gray
    done
  done

let setup () =
  init_window 1000 900 "Minesweeper";
  set_mouse_scale 1. 1.

let rec loop () =
  if Raylib.window_should_close () then Raylib.close_window ()
  else
    let open Raylib in
    begin_drawing ();
    clear_background Color.raywhite;
    draw_text "minesweeper? i hardly know her!" 350 450 20 Color.black;
    drawGrid (Board.newboard 10 10);
    end_drawing ();
    loop ()

let () = setup () |> loop
