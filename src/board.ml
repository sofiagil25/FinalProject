(* exceptions here *)

(* Type describing one box of the board *)
type box = {
  row : int;
  col : int;
  bomb : int;
  count : int;
  mutable flag : bool;
  mutable obstacle : bool;
  mutable solution : bool;
}

type board = { base : box array array }

(* val tobox : int -> int -> box *)
let tobox (x : int) (y : int) (value : int) (count : int) (boo : bool) =
  {
    row = x;
    col = y;
    bomb = value;
    count = 0;
    flag = boo;
    obstacle = false;
    solution = false;
  }

(* prob is a value from 0 to 100 representing the probability that any one
   square is a bomb *)
let bomborno prob =
  Random.self_init ();
  if Random.int 100 < prob then -1 else 0

let numbombs board =
  let totheight = Array.length board in
  let totwidth = Array.length (Array.get board 0) in
  let count = ref 0 in
  let xind = ref 0 in
  let yind = ref 0 in
  while !xind < totwidth do
    while !yind < totheight do
      if (Array.get (Array.get board !yind) !xind).bomb == -1 then
        count := !count + 1;
      yind := !yind + 1
    done;
    yind := 0;
    xind := !xind + 1
  done;
  !count

let percentofboard board =
  let totlength = Array.length board in
  let totheight = Array.length (Array.get board 0) in
  let area = totlength * totheight in
  let numbombs = numbombs board in
  (* let _ = print_string "area = " in let _ = print_int area in let _ =
     print_string " nb = " in let _ = print_int numbombs in let _ = print_string
     (string_of_bool (0.0 < 0.5)) in *)
  float_of_int numbombs /. float_of_int area

(* obsorno tells us if we are placing an obstable for a given board *)
let obsorno board = if percentofboard board < 0.5 then true else false

let rec placeobs (bor : box array array) =
  (* if the board is more than 50% bombs, dont do this *)
  if obsorno bor then (
    Random.self_init ();
    let lengthx = Array.length bor in
    let lengthy = Array.length (Array.get bor 0) in
    let obsxcoord = Random.int lengthx in
    let obsycoord = Random.int lengthy in
    let _ = print_string "x coord = " in
    let _ = print_int obsxcoord in
    let _ = print_string " y coord = " in
    let _ = print_int obsycoord in
    let bx = Array.get (Array.get bor obsxcoord) obsycoord in
    match bx with
    | {
     row = obsxcoord;
     col = obsycoord;
     bomb = 0;
     count = _;
     flag = _;
     obstacle = false;
     solution = false;
    } -> bx.obstacle <- true
    | _ -> placeobs bor)

let rec placesol (bor : box array array) =
  if obsorno bor then (
    Random.self_init ();
    let lengthx = Array.length bor in
    let lengthy = Array.length (Array.get bor 0) in
    let obsxcoord = Random.int lengthx in
    let obsycoord = Random.int lengthy in
    (* let _ = print_int obsxcoord in let _ = print_int obsycoord in *)
    let bx = Array.get (Array.get bor obsxcoord) obsycoord in
    match bx with
    | {
     row = obsxcoord;
     col = obsycoord;
     bomb = 0;
     count = _;
     flag = _;
     obstacle = false;
     solution = false;
    } -> bx.solution <- true
    | _ -> placesol bor)

(* getobs box is a returns whether a box is an obstacle *)
let getobs (box : box) = box.obstacle

(* getsol box is a returns whether a box is an solution *)
let getsol (box : box) = box.solution

(* makes a new empty line of boxes, not bombs at row height *)
let newemptyline height (length : int) prob : box array =
  Array.init length (fun i ->
      {
        row = height;
        col = i;
        bomb = bomborno prob;
        count = 0;
        flag = false;
        obstacle = false;
        solution = false;
      })

(* [height] rows of length [width]. so, newboard[height][width] is how to access
   elements *)
let rec newboard width height prob : box array array =
  Array.init height (fun i -> newemptyline i width prob)

let ismine (game : box array array) x y = game.(x).(y).bomb
let isminebool game x y = if ismine game x y < 0 then true else false
let getbox col row board = Array.get (Array.get board row) col
let getcount (game : box array array) x y = game.(x).(y).count
let getflag (game : box array array) x y = game.(x).(y).flag
let setflag (game : box array array) x y = game.(x).(y).flag <- true

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
            obstacle = false;
            solution = false;
          })
        row)
    board

(* samesize b1 b2 returns true if b1 and b2 are the same size, false otherwise.
   Assumes the rows in a board are all the same length *)
let samesize (b1 : box array array) (b2 : box array array) =
  Array.length b1 == Array.length b2
  && Array.length (Array.get b1 0) == Array.length (Array.get b2 0)

let boxequal (b1 : box) (b2 : box) =
  b1.flag == b2.flag && b1.col == b2.col && b1.row == b2.row
  && b1.bomb == b2.bomb && b1.count == b2.count && b1.obstacle == b2.obstacle
(* && b1.solution == b2.solution *)

(* returns true if two rows of boxes are equal *)
let rowequal (r1 : box array) (r2 : box array) (equalfun : box -> box -> bool) =
  if not (Array.length r1 == Array.length r2) then false
  else
    let x = ref 0 in
    let vali = ref true in
    while !x < Array.length r1 && !vali do
      vali := equalfun (Array.get r1 !x) (Array.get r2 !x);
      x := !x + 1
    done;
    !vali

let simpleequal (b1 : box) (b2 : box) = b1.flag == b2.flag && b1.bomb == b2.bomb

(* isboardequalsquestionmark board1 board2 checks that b1 and b2 are the same
   box array array. Assumes all the rows in each array are the same length *)
let isboardsequalquestionmarksimple (b1 : box array array)
    (b2 : box array array) =
  if samesize b1 b2 then (
    let row = ref 0 in
    let vali = ref true in
    while !row < Array.length b1 && !vali do
      vali := rowequal (Array.get b1 !row) (Array.get b2 !row) simpleequal;
      row := !row + 1
    done;
    !vali)
  else false

(* isboardequalsquestionmark board1 board2 checks that b1 and b2 are the same
   box array array. Assumes all the rows in each array are the same length *)
let isboardsequalquestionmark (b1 : box array array) (b2 : box array array) =
  if samesize b1 b2 then (
    let row = ref 0 in
    let vali = ref true in
    while !row < Array.length b1 && !vali do
      vali := rowequal (Array.get b1 !row) (Array.get b2 !row) boxequal;
      row := !row + 1
    done;
    !vali)
  else false

(* let *)
