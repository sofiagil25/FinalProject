(* Type describing one box of the board *)
type box
type board

(* [tobox x y value] returns a box representation of this square. (x,y) is an
   int value representing the box's location in the game. [value] is -1 if the
   box is a bomb, the number of bombs the box is touching if not *)
val tobox : int -> int -> int -> box

(* [newboard width height prob] will return a box list list with dimensions
   width and height where each square is either a bomb or not a bomb with
   probability prob*)
val newboard : int -> int -> int -> box array array
val ismine : box array array -> int -> int -> int
val getval : box -> int
val getcount : box array array -> int -> int -> int
val boardwithvalue : box array array -> box array array
