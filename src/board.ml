(* exceptions here *)

(* Type describing one box of the board *)
type box = {
  row : int;
  col : int;
  bomb : int;
}

type board = { base : box list list }

(* val tobox : int -> int -> box *)
let tobox (x : int) (y : int) (value : int) = { row = x; col = y; bomb = value }

(* makes a new empty line of boxes, not bombs at row height *)
let newemptyline height (length : int) : box array =
  Array.init length (fun i -> { row = height; col = i; bomb = 0 })

let rec newboard () width height (acc : box array array) : box array array =
  Array.init height (fun i -> newemptyline i width)

(* new line of values: -1 for bomb, 0 for not bomb let rec line height dim prob
   acc : box list = match dim with | 0 -> acc | x -> ( match Random.int 10 with
   | 0 -> line height (dim - 1) prob ({ row = dim; col = height; bomb = -1 } ::
   acc) | y -> line height (dim - 1) prob ({ row = x; col = y; bomb = 0 } ::
   acc))

   (* new game : width x height *) let rec newboard width height acc : box list
   list = if height > 0 then newboard width (height - 1) (line height width 10
   [] :: acc) else acc *)
