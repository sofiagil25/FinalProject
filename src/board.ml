(* exceptions here *)

(* Type describing one box of the board *)
type box = {
  row : int;
  col : int;
  bomb : int;
  count : int;
  mutable flag : bool;
}

type board = { base : box array array }

(* val tobox : int -> int -> box *)
let tobox (x : int) (y : int) (value : int) =
  { row = x; col = y; bomb = value; count = 0; flag = false }

(* prob is a value from 0 to 100 representing the probability that any one
   square is a bomb *)
let bomborno prob =
  Random.self_init ();
  if Random.int 100 < prob then -1 else 0

(* makes a new empty line of boxes, not bombs at row height *)
let newemptyline height (length : int) prob : box array =
  Array.init length (fun i ->
      { row = height; col = i; bomb = bomborno prob; count = 0; flag = false })

(* [height] rows of length [width]. so, newboard[height][width] is how to access
   elements *)
let rec newboard width height prob : box array array =
  Array.init height (fun i -> newemptyline i width prob)

let ismine (game : box array array) x y = game.(x).(y).bomb
let isminebool game x y = if ismine game x y < 0 then true else false
let getval (b : box) = b.bomb
let getbox col row board = Array.get (Array.get board row) col
let getcount (game : box array array) x y = game.(x).(y).count
let getflag (game : box array array) x y = game.(x).(y).flag
let setflag (game : box array array) x y = game.(x).(y).flag <- true

(* let boardwbomb board prob = *)

(* new line of values: -1 for bomb, 0 for not bomb *)
(* let rec line height dim prob acc : box list = match dim with | 0 -> acc | x
   -> ( match Random.int 10 with | 0 -> line height (dim - 1) prob ({ row = dim;
   col = height; bomb = -1 } :: acc) | y -> line height (dim - 1) prob ({ row =
   x; col = y; bomb = 0 } :: acc))

   (* new game : width x height *) let rec newboard width height acc : box list
   list = if height > 0 then newboard width (height - 1) (line height width 10
   [] :: acc) else acc *)

let countup board col row acc =
  try if isminebool board row col then acc + 1 else acc with _ -> acc

let onecount (boxi : box) boardi acc =
  countup boardi (boxi.col - 1) (boxi.row - 1) acc
  |> countup boardi (boxi.col - 1) boxi.row
  |> countup boardi (boxi.col - 1) (boxi.row + 1)
  |> countup boardi boxi.col (boxi.row + 1)
  |> countup boardi (boxi.col + 1) (boxi.row + 1)
  |> countup boardi (boxi.col + 1) (boxi.row - 1)
  |> countup boardi (boxi.col + 1) boxi.row
  |> countup boardi boxi.col (boxi.row - 1)

(* countup boardi (boxi.col - 1) (boxi.row - 1) acc |> countup boardi (boxi.col
   - 1) boxi.row |> countup boardi (boxi.col - 1) (boxi.row + 1) |> countup
   boardi boxi.col (boxi.row + 1) |> countup boardi (boxi.col + 1) (boxi.row +
   1) |> countup boardi (boxi.col + 1) (boxi.row - 1) |> countup boardi
   (boxi.col + 1) boxi.row |> countup boardi boxi.col (boxi.row - 1) *)

let boardwithvalue (board : box array array) =
  Array.map
    (fun row ->
      Array.map
        (fun box ->
          {
            row = box.row;
            col = box.col;
            bomb = box.bomb;
            count = onecount box board 0;
            flag = false;
          })
        row)
    board
