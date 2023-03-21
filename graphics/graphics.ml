open Raylib

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
    end_drawing ();
    loop ()

let () = setup () |> loop
