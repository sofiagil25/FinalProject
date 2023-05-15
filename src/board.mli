(* Type describing one box of the board *)
type box
type board

(* [tobox x y bomb count flag] returns a box representation of this square.
   (x,y) is an int value representing the box's location in the game. [value] is
   -1 if the box is a bomb, the number of bombs the box is touching if not *)
val tobox : int -> int -> int -> int -> bool -> box

(* [newboard width height prob] returns a box array array with dimensions width
   and height, where each square is either a bomb or not a bomb with probability
   prob. Requires: prob be an int between 0 and 100, inclusive*)
val newboard : int -> int -> int -> box array array

(* [ismine board x y] returns -1 if the box at location x y is a bomb, 0
   elsewise *)
val ismine : box array array -> int -> int -> int

(* [getcount board x y] returns the count of the box as location (x,y) in board,
   where count is the number of bombs touching the box. Required: x < height
   board, y < length board*)
val getcount : box array array -> int -> int -> int

(* [getobs box] returns whether the box is an obstacle*)
val getobs : box -> bool

(* [getsol box] returns whether the box is an solution*)
val getsol : box -> bool

(* [boardwithvalue board] returns a new board with the same boxes as board, but
   with count updated to reflect the bomb placements*)
val boardwithvalue : box array array -> box array array

(* [getflag board x y] returns a boolean value representing whether the box at
   position (x,y) is flagged*)
val getflag : box array array -> int -> int -> bool

(* [setflag board x y] sets the box at position (x,y) as flagged*)
val setflag : box array array -> int -> int -> unit

(* [isboardequalquesionmark boarda boardb] returns a boolean value representing
   whether boarda and boardb are fully equal (as defined in main.ml)*)
val isboardsequalquestionmark : box array array -> box array array -> bool

(* [isboardequalquesionmarksimple boarda boardb] returns a boolean value
   representing whether boarda and boardb are simply equal (as defined in
   main.ml)*)
val isboardsequalquestionmarksimple : box array array -> box array array -> bool

(* [placeobs boarda] places an obstacle on boarda, provided less than 50% of the
   board is bombs*)
val placeobs : box array array -> unit

(* [placesol boarda] places a solution on boarda, provided less than 50% of the
   board is bombs*)
val placesol : box array array -> unit

(* [printboard boarda] prints a string representation of the board *)
val printboard : box array array -> unit

(* [issolution board x y] returns a bool representing whether the box at
   position (x,y) in board is a solution*)
val issolution : board -> int -> int -> bool

(* [isobstacle board x y] returns a bool representing whether the box at
   position (x,y) in board is an obstacle*)
val isobstacle : board -> int -> int -> bool

(* [makeboard board] returns a Board value, when give a box array array*)
val makeboard : box array array -> board

(* [to_string_count board] returns a string representation of the board with its
   count value *)
val to_string_count : box array array -> string
(* [to_string_flag board] returns a string representation of the board with its
   flag value *)

val to_string_flag : box array array -> string
