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
let boxWidth:int = 20
let xCoord= ref 0
let yCoord= ref 0
let rec drawGrid m =  
  for i=0 to Array.length m-1 do 
    for j=0 to Array.length (Array.get m i) do 
      if (Array.get (Array.get m i) j) = -1 then draw_rectangle !xCoord !yCoord boxWidth boxWidth Color.red 
      else draw_rectangle !xCoord !yCoord boxWidth boxWidth Color.gray;
      yCoord:= !yCoord + boxWidth
    done;
  xCoord:= !xCoord + boxWidth
done;






