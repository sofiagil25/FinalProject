(* Type describing one box of the board *)
type box
type board

(* [tobox x y bomb count flag] returns a box representation of this square.
   (x,y) is an int value representing the box's location in the game. [value] is
   -1 if the box is a bomb, the number of bombs the box is touching if not *)
val tobox : int -> int -> int -> int -> bool -> box

(* [newboard width height prob] will return a box list list with dimensions
   width and height where each square is either a bomb or not a bomb with
   probability prob*)
val newboard : int -> int -> int -> box array array

(* ismine board x y returns -1 if the box at location x y is a bomb, 0
   elsewise *)
val ismine : box array array -> int -> int -> int
(* getval *)
(* val getval : box -> int *)

val getcount : box array array -> int -> int -> int
val getobs : box -> bool
val getsol : box -> bool
val boardwithvalue : box array array -> box array array
val getflag : box array array -> int -> int -> bool
val setflag : box array array -> int -> int -> unit
val isboardsequalquestionmark : box array array -> box array array -> bool
val isboardsequalquestionmarksimple : box array array -> box array array -> bool
val placeobs : box array array -> unit
val placesol : box array array -> unit
val printboard : box array array -> unit
val issolution : board -> int -> int -> bool
val isobstacle : board -> int -> int -> bool
