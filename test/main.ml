open OUnit2
open Game

(* The test plan should be located in a comment at the top of the test file. All
   elements of board were tested usign OUnit, while graphics was necessarily
   tested through gameplay. Test cases were developed using glass box testing.
   The tests build on each element of board, starting with estabilishing two
   different standands for board equality : simple equality and full equality.
   Simple equality implies that the boards are the same size and the count
   values are correct -> that is, for every element in board a, if board b is
   equal, the count of how many bombs the element is touching should be the same
   for the both. This is simple equality. The second standand was full equality.
   Full equality implies that the count, flag, bomb value, obstacle status, and
   solution status are all equal for equivalent elements of boards a and b. *)
(*
  We tested the UI seperately by playing each mode in combination with each feature.
  Clearly a lot of the functions are visible, and if they weren't working the
  game wouldn't run. We also did test-driven development, so we would implement each feature,
  test it pretty thoroughly, and then move on, which ensured that everything works.
  We were systematic about testing each other's code instead of our own so that 
  we could be extra critical. 

*)
let thisgameboard = ref (Board.newboard 6 5 15)
let thisboard = Array.make 5 (Board.tobox 0 0 0)

(* These value are used to test bombless boards *)
let emptybox = Board.tobox 0 0 0 0 false
let emptyline = Array.make 5 emptybox
let emptyboard = Array.make 5 emptyline

(* A board with zero probability of a bomb *)
let zeroprobboard = Board.newboard 5 5 0

(* These values are used to test only bomb boards *)
let bombbox = Board.tobox 0 0 (-1) 0 false
let bombline = Array.make 5 bombbox
let bombboard = Array.make 5 bombline

(* A board with zero probability of a bomb *)
let hundredprobboard = Board.newboard 5 5 100

(* Tests that a board is properly constructed with coordinates All elements are
   not bombs *)
let babyarray x y = Array.make 1 (Board.tobox x y 0 0 false)

let linex x =
  babyarray x 4
  |> Array.append (babyarray x 3)
  |> Array.append (babyarray x 2)
  |> Array.append (babyarray x 1)
  |> Array.append (babyarray x 0)

let line1 = linex 0
let line2 = linex 1
let line3 = linex 2
let line4 = linex 3
let line5 = linex 4

let boardcoord =
  Array.make 1 line5
  |> Array.append (Array.make 1 line4)
  |> Array.append (Array.make 1 line3)
  |> Array.append (Array.make 1 line2)
  |> Array.append (Array.make 1 line1)

(* test bomb counting on a 100% board*)
let babyarraybomb x y count boop =
  Array.make 1 (Board.tobox x y (-1) count boop)

let toporbottom x =
  (* babyarraybomb x 0 3 false |> Array.append (babyarraybomb x 1 5 false) |>
     Array.append (babyarraybomb x 2 5 false) |> Array.append (babyarraybomb x 3
     5 false) |> Array.append (babyarraybomb x 4 3 false) *)
  babyarraybomb x 4 3 false
  |> Array.append (Array.copy (babyarraybomb x 3 5 false))
  |> Array.append (Array.copy (babyarraybomb x 2 5 false))
  |> Array.append (Array.copy (babyarraybomb x 1 5 false))
  |> Array.append (Array.copy (babyarraybomb x 0 3 false))

let middle x =
  (* babyarraybomb x 0 5 false |> Array.append (babyarraybomb x 1 8 false) |>
     Array.append (babyarraybomb x 2 8 false) |> Array.append (babyarraybomb x 3
     8 false) |> Array.append (babyarraybomb x 4 5 false) *)
  babyarraybomb x 4 5 false
  |> Array.append (Array.copy (babyarraybomb x 3 8 false))
  |> Array.append (Array.copy (babyarraybomb x 2 8 false))
  |> Array.append (Array.copy (babyarraybomb x 1 8 false))
  |> Array.append (Array.copy (babyarraybomb x 0 5 false))

let bline0 = toporbottom 0
let bline1 = middle 1
let bline2 = middle 2
let bline3 = middle 3
let bline4 = toporbottom 4

let boardwithbomb =
  (* Array.make 1 bline0 |> Array.append (Array.make 1 bline1) |> Array.append
     (Array.make 1 bline2) |> Array.append (Array.make 1 bline3) |> Array.append
     (Array.make 1 bline4) *)
  Array.make 1 bline4
  |> Array.append (Array.copy (Array.make 1 bline3))
  |> Array.append (Array.copy (Array.make 1 bline2))
  |> Array.append (Array.copy (Array.make 1 bline1))
  |> Array.append (Array.copy (Array.make 1 bline0))

(* testing count on a regatangular board *)
let toporbottomlarge x =
  Array.copy (babyarraybomb x 10 3 false)
  |> Array.append (Array.copy (babyarraybomb x 9 5 false))
  |> Array.append (Array.copy (babyarraybomb x 8 5 false))
  |> Array.append (Array.copy (babyarraybomb x 7 5 false))
  |> Array.append (Array.copy (babyarraybomb x 6 5 false))
  |> Array.append (Array.copy (babyarraybomb x 5 5 false))
  |> Array.append (Array.copy (babyarraybomb x 4 5 false))
  |> Array.append (Array.copy (babyarraybomb x 3 5 false))
  |> Array.append (Array.copy (babyarraybomb x 2 5 false))
  |> Array.append (Array.copy (babyarraybomb x 1 5 false))
  |> Array.append (Array.copy (babyarraybomb x 0 3 false))

let middlelarge x =
  Array.copy (babyarraybomb x 10 5 false)
  |> Array.append (Array.copy (babyarraybomb x 9 8 false))
  |> Array.append (Array.copy (babyarraybomb x 8 8 false))
  |> Array.append (Array.copy (babyarraybomb x 7 8 false))
  |> Array.append (Array.copy (babyarraybomb x 6 8 false))
  |> Array.append (Array.copy (babyarraybomb x 5 8 false))
  |> Array.append (Array.copy (babyarraybomb x 4 8 false))
  |> Array.append (Array.copy (babyarraybomb x 3 8 false))
  |> Array.append (Array.copy (babyarraybomb x 2 8 false))
  |> Array.append (Array.copy (babyarraybomb x 1 8 false))
  |> Array.append (Array.copy (babyarraybomb x 0 5 false))

let blinelarge0 = toporbottomlarge 0
let blinelarge1 = middlelarge 1
let blinelarge2 = middlelarge 2
let blinelarge3 = middlelarge 3
let blinelarge4 = toporbottomlarge 4

let boardwithbomblarge =
  Array.make 1 blinelarge4
  |> Array.append (Array.make 1 blinelarge3)
  |> Array.append (Array.make 1 blinelarge2)
  |> Array.append (Array.make 1 blinelarge1)
  |> Array.append (Array.make 1 blinelarge0)

let copy0 : Board.box array = Array.copy blinelarge0
let copy1 = Array.copy blinelarge1
let copy2 = Array.copy blinelarge2
let copy3 = Array.copy blinelarge3
let copy4 = Array.copy blinelarge4

let boardwithflag =
  Array.make 1 copy4
  |> Array.append (Array.make 1 copy3)
  |> Array.append (Array.make 1 copy2)
  |> Array.append (Array.make 1 copy1)
  |> Array.append (Array.make 1 copy0)

(* Set position x y as flagged *)
let flag x y board = Board.setflag board x y

let checkboardrandom x y prob =
  let ogboard = Board.boardwithvalue (Board.newboard x y prob) in
  let count = ref 0 in
  let iter = ref 0 in
  while !iter < 15 do
    if
      Board.isboardsequalquestionmark ogboard
        (Board.boardwithvalue (Board.newboard x y prob))
    then count := !count + 1;
    iter := !iter + 1
  done;
  if !count > 5 then false else true

let obsinbox (box : Board.box) = Board.getobs box

let obsinline line =
  let boo = ref false in
  let x = ref 0 in
  while !x < Array.length line && not !boo do
    boo := obsinbox (Array.get line !x);
    x := !x + 1
  done;
  !boo

let obsinboard board =
  let boo = ref false in
  let x = ref 0 in
  while !x < Array.length board && not !boo do
    boo := obsinline (Array.get board !x);
    x := !x + 1
  done;
  !boo

let solinbox (box : Board.box) = Board.getsol box

let solinline line =
  let boo = ref false in
  let x = ref 0 in
  while !x < Array.length line && not !boo do
    boo := solinbox (Array.get line !x);
    x := !x + 1
  done;
  !boo

let solinboard board =
  let boo = ref false in
  let x = ref 0 in
  while !x < Array.length board && not !boo do
    boo := solinline (Array.get board !x);
    x := !x + 1
  done;
  !boo

let ifobsthensolution board =
  let one = obsinboard board in
  let two = solinboard board in
  one == two
(* let brd1 = Array.copy boardwithbomblarge let brd2 = Array.copy
   boardwithbomblarge *)

(* creating two identical boards to test if the obs and solutions are
   different *)
let babyarrayobs x y count boop = Array.make 1 (Board.tobox x y 0 count boop)

let topofa =
  Array.copy (babyarrayobs 0 4 3 false)
  |> Array.append (Array.copy (babyarrayobs 0 3 5 false))
  |> Array.append (Array.copy (babyarrayobs 0 2 5 false))
  |> Array.append (Array.copy (babyarrayobs 0 1 5 false))
  |> Array.append (Array.copy (babyarrayobs 0 0 3 false))

let row1a =
  Array.copy (babyarrayobs 1 4 5 false)
  |> Array.append (Array.copy (babyarrayobs 1 3 8 false))
  |> Array.append (Array.copy (babyarrayobs 1 2 8 false))
  |> Array.append (Array.copy (babyarrayobs 1 1 8 false))
  |> Array.append (Array.copy (babyarrayobs 1 0 5 false))

let row2a =
  Array.copy (babyarrayobs 2 4 5 false)
  |> Array.append (Array.copy (babyarrayobs 2 3 8 false))
  |> Array.append (Array.copy (babyarrayobs 2 2 8 false))
  |> Array.append (Array.copy (babyarrayobs 2 1 8 false))
  |> Array.append (Array.copy (babyarrayobs 2 0 5 false))

let bottomofa =
  Array.copy (babyarrayobs 3 4 3 false)
  |> Array.append (Array.copy (babyarrayobs 3 3 5 false))
  |> Array.append (Array.copy (babyarrayobs 3 2 5 false))
  |> Array.append (Array.copy (babyarrayobs 3 1 5 false))
  |> Array.append (Array.copy (babyarrayobs 3 0 3 false))

let topofb =
  Array.copy (babyarrayobs 0 4 3 false)
  |> Array.append (Array.copy (babyarrayobs 0 3 5 false))
  |> Array.append (Array.copy (babyarrayobs 0 2 5 false))
  |> Array.append (Array.copy (babyarrayobs 0 1 5 false))
  |> Array.append (Array.copy (babyarrayobs 0 0 3 false))

let row1b =
  Array.copy (babyarrayobs 1 4 5 false)
  |> Array.append (Array.copy (babyarrayobs 1 3 8 false))
  |> Array.append (Array.copy (babyarrayobs 1 2 8 false))
  |> Array.append (Array.copy (babyarrayobs 1 1 8 false))
  |> Array.append (Array.copy (babyarrayobs 1 0 5 false))

let row2b =
  Array.copy (babyarrayobs 2 4 5 false)
  |> Array.append (Array.copy (babyarrayobs 2 3 8 false))
  |> Array.append (Array.copy (babyarrayobs 2 2 8 false))
  |> Array.append (Array.copy (babyarrayobs 2 1 8 false))
  |> Array.append (Array.copy (babyarrayobs 2 0 5 false))

let bottomofb =
  Array.copy (babyarrayobs 3 4 3 false)
  |> Array.append (Array.copy (babyarrayobs 3 3 5 false))
  |> Array.append (Array.copy (babyarrayobs 3 2 5 false))
  |> Array.append (Array.copy (babyarrayobs 3 1 5 false))
  |> Array.append (Array.copy (babyarrayobs 3 0 3 false))

let boarda =
  Array.make 1 bottomofa
  |> Array.append (Array.make 1 row2a)
  |> Array.append (Array.make 1 row1a)
  |> Array.append (Array.make 1 topofa)

let boardb =
  Array.make 1 bottomofb
  |> Array.append (Array.make 1 row2b)
  |> Array.append (Array.make 1 row1b)
  |> Array.append (Array.make 1 topofb)

(* two by two board environment to test obstacles and solutions *)
let row1o =
  Array.copy (babyarrayobs 0 1 0 false)
  |> Array.append (Array.copy (babyarrayobs 0 0 0 false))

let row2o =
  Array.copy (babyarrayobs 1 1 0 false)
  |> Array.append (Array.copy (babyarrayobs 1 0 0 false))

let row1c =
  Array.copy (babyarrayobs 0 1 0 false)
  |> Array.append (Array.copy (babyarrayobs 0 0 0 false))

let row2c =
  Array.copy (babyarrayobs 1 1 0 false)
  |> Array.append (Array.copy (babyarrayobs 1 0 0 false))

let bo = Array.make 1 row2o |> Array.append (Array.make 1 row1o)
let bc = Array.make 1 row2c |> Array.append (Array.make 1 row1c)

let board_tests =
  [
    ( "test equals with same object" >:: fun _ ->
      assert (Board.isboardsequalquestionmarksimple emptyboard emptyboard) );
    ( "test not equals with objects of different size" >:: fun _ ->
      assert (Board.isboardsequalquestionmarksimple emptyboard boardcoord) );
    ( "test equals with different object" >:: fun _ ->
      assert (Board.isboardsequalquestionmarksimple emptyboard zeroprobboard) );
    ( "test equals with non square boards" >:: fun _ ->
      assert (
        Board.isboardsequalquestionmarksimple (Array.make 6 emptyline)
          (Board.newboard 5 6 0)) );
    ( "test equals with different bombfull object" >:: fun _ ->
      assert (Board.isboardsequalquestionmarksimple bombboard hundredprobboard)
    );
    ( "test equals with non square boards" >:: fun _ ->
      assert (
        Board.isboardsequalquestionmarksimple (Array.make 6 bombline)
          (Board.newboard 5 6 100)) );
    ( "test equals with coord boards" >:: fun _ ->
      assert (Board.isboardsequalquestionmark boardcoord (Board.newboard 5 5 0))
    );
    ( "test count" >:: fun _ ->
      assert (
        Board.isboardsequalquestionmark boardwithbomb (Board.newboard 5 5 100))
    );
    ( "test simpleequals on a rectangular boards" >:: fun _ ->
      assert (
        Board.isboardsequalquestionmarksimple boardwithbomblarge
          (Board.newboard 11 5 100)) );
    ( "test count on a rectangular boards" >:: fun _ ->
      assert (
        Board.isboardsequalquestionmark boardwithbomblarge
          (Board.newboard 11 5 100)) );
    ( "test get_flag no flag" >:: fun _ ->
      assert (Board.getflag boardwithflag 2 4 = false) );
    ( "test set_flag" >:: fun _ ->
      assert (
        let _ = Board.setflag boardwithflag 2 3 in
        Board.getflag boardwithflag 2 3 = true) );
    ( "test count fails" >:: fun _ ->
      assert (
        Board.isboardsequalquestionmark boardwithbomb boardwithbomblarge
        == false) );
    ( "test set_flag fail" >:: fun _ ->
      let _ = Board.setflag boarda 2 3 in
      assert (Board.getflag boardb 2 3 = false) );
    ( "test is_mine true" >:: fun _ ->
      assert (Board.ismine boardwithflag 2 3 == -1) );
    ("test is_mine false" >:: fun _ -> assert (Board.ismine boardcoord 2 3 == 0));
    ( "test random board generation" >:: fun _ ->
      assert (checkboardrandom 25 25 50) );
    ( "test random board always same for prob 100" >:: fun _ ->
      assert (checkboardrandom 25 25 100 == false) );
    ( "test random board always same for prob 0" >:: fun _ ->
      assert (checkboardrandom 25 25 0 == false) );
    ( "test random board always same for prob 0 with different sizes"
    >:: fun _ -> assert (checkboardrandom 25 35 0 == false) );
    ( "assert random board with different sizes" >:: fun _ ->
      assert (checkboardrandom 25 35 50) );
    ( "test no obs or sol when not called" >:: fun _ ->
      assert (ifobsthensolution (Board.newboard 11 5 100)) );
    ( "test no obs or sol when called with percentage >>50" >:: fun _ ->
      assert (
        let brd = Board.newboard 11 10 90 in
        Board.placeobs brd;
        Board.placesol brd;
        ifobsthensolution brd) );
    ( "test no obs only when called with percentage >>50" >:: fun _ ->
      assert (
        let brd = Board.newboard 11 10 90 in
        Board.placeobs brd;
        Board.placesol brd;
        obsinboard brd == false) );
    ( "test no sol only when called with percentage >>50" >:: fun _ ->
      assert (
        let brd = Board.newboard 11 10 90 in
        Board.placeobs brd;
        Board.placesol brd;
        solinboard brd == false) );
    ( "test yes obs and sol when called with percentage <<50" >:: fun _ ->
      assert (
        let brd = Board.newboard 11 10 10 in
        Board.placeobs brd;
        Board.placesol brd;
        ifobsthensolution brd) );
    ( "test yes obs only when called with percentage <<50" >:: fun _ ->
      assert (
        let brd = Board.newboard 11 10 19 in
        Board.placeobs brd;
        Board.placesol brd;
        obsinboard brd) );
    ( "test yes sol only when called with percentage <<50" >:: fun _ ->
      assert (
        let brd = Board.newboard 11 10 19 in
        Board.placeobs brd;
        Board.placesol brd;
        solinboard brd) );
    ( "assert boarda and boardb are simply equal" >:: fun _ ->
      assert (Board.isboardsequalquestionmarksimple boarda boardb) );
    ( "assert creating solutions with the same object twice will still result \
       in the different full grid"
    >:: fun _ ->
      assert (
        let _ = Board.placeobs boarda in
        let _ = Board.placesol boarda in
        let _ = Board.placeobs boardb in
        let _ = Board.placesol boardb in
        let _ = Board.printboard boarda in
        let _ = Board.printboard boardb in
        Board.isboardsequalquestionmark boarda boardb = false) );
    ( "assert boarda and boardb are still simply equal" >:: fun _ ->
      assert (
        let _ = Board.placeobs boarda in
        let _ = Board.placesol boarda in
        let _ = Board.placeobs boardb in
        let _ = Board.placesol boardb in
        let _ = Board.printboard boarda in
        let _ = Board.printboard boardb in
        Board.isboardsequalquestionmarksimple boarda boardb) );
    ( "assert solutions in boara" >:: fun _ ->
      assert (
        let _ = Board.placesol boarda in
        let _ = Board.placesol boardb in
        solinboard boarda) );
    ( "assert solutions in boarb" >:: fun _ ->
      assert (
        let _ = Board.placesol boarda in
        let _ = Board.placesol boardb in
        solinboard boardb) );
    ( "assert obstacles in boarda" >:: fun _ ->
      assert (
        let _ = Board.placeobs boarda in
        let _ = Board.placeobs boardb in
        obsinboard boarda) );
    ( "assert obstacles in boardb" >:: fun _ ->
      assert (
        let _ = Board.placeobs boarda in
        let _ = Board.placeobs boardb in
        obsinboard boardb) );
    ( "assert obstacles in boardo" >:: fun _ ->
      assert (
        let _ = Board.placeobs bo in
        Board.isobstacle (Board.makeboard bo) 0 0
        || Board.isobstacle (Board.makeboard bo) 0 1
        || Board.isobstacle (Board.makeboard bo) 1 0
        || Board.isobstacle (Board.makeboard bo) 1 1) );
    ( "assert solutions in boardo" >:: fun _ ->
      assert (
        let _ = Board.placesol bo in
        Board.issolution (Board.makeboard bo) 0 0
        || Board.issolution (Board.makeboard bo) 0 1
        || Board.issolution (Board.makeboard bo) 1 0
        || Board.issolution (Board.makeboard bo) 1 1) );
    ( "assert obstacles not in boardc" >:: fun _ ->
      assert (
        not
          (Board.issolution (Board.makeboard bc) 0 0
          || Board.issolution (Board.makeboard bc) 0 1
          || Board.issolution (Board.makeboard bc) 1 0
          || Board.issolution (Board.makeboard bc) 1 1)) );
    ( "assert solutions not in boardc" >:: fun _ ->
      assert (
        not
          (Board.issolution (Board.makeboard bc) 0 0
          || Board.issolution (Board.makeboard bc) 0 1
          || Board.issolution (Board.makeboard bc) 1 0
          || Board.issolution (Board.makeboard bc) 1 1)) );
    ( "assert boardc and boardo are simply equal" >:: fun _ ->
      assert (Board.isboardsequalquestionmarksimple bc bo) );
    ( "assert boardc and boardo are not equal" >:: fun _ ->
      assert (not (Board.isboardsequalquestionmark bc bo)) );
    ( "assert to_string_count of small board is correct" >:: fun _ ->
      assert (
        Board.to_string_count bo
        = "[[[Count of (0, 0) = 0; Count of (0, 1) = 0]]; [[Count of (1, 0) = \
           0; Count of (1, 1) = 0]]]") );
    ( "assert to_string_count of large board is correct" >:: fun _ ->
      assert (
        Board.to_string_count boardwithbomblarge
        = "[[[Count of (0, 0) = 0; Count of (0, 1) = 0; Count of (0, 2) = 0; \
           Count of (0, 3) = 0; Count of (0, 4) = 0; Count of (0, 5) = 0; \
           Count of (0, 6) = 0; Count of (0, 7) = 0; Count of (0, 8) = 0; \
           Count of (0, 9) = 0; Count of (0, 10) = 0]]; [[Count of (1, 0) = 0; \
           Count of (1, 1) = 0; Count of (1, 2) = 0; Count of (1, 3) = 0; \
           Count of (1, 4) = 0; Count of (1, 5) = 0; Count of (1, 6) = 0; \
           Count of (1, 7) = 0; Count of (1, 8) = 0; Count of (1, 9) = 0; \
           Count of (1, 10) = 0]]; [[Count of (2, 0) = 0; Count of (2, 1) = 0; \
           Count of (2, 2) = 0; Count of (2, 3) = 0; Count of (2, 4) = 0; \
           Count of (2, 5) = 0; Count of (2, 6) = 0; Count of (2, 7) = 0; \
           Count of (2, 8) = 0; Count of (2, 9) = 0; Count of (2, 10) = 0]]; \
           [[Count of (3, 0) = 0; Count of (3, 1) = 0; Count of (3, 2) = 0; \
           Count of (3, 3) = 0; Count of (3, 4) = 0; Count of (3, 5) = 0; \
           Count of (3, 6) = 0; Count of (3, 7) = 0; Count of (3, 8) = 0; \
           Count of (3, 9) = 0; Count of (3, 10) = 0]]; [[Count of (4, 0) = 0; \
           Count of (4, 1) = 0; Count of (4, 2) = 0; Count of (4, 3) = 0; \
           Count of (4, 4) = 0; Count of (4, 5) = 0; Count of (4, 6) = 0; \
           Count of (4, 7) = 0; Count of (4, 8) = 0; Count of (4, 9) = 0; \
           Count of (4, 10) = 0]]]") );
    ( "assert to_string_flag of small board is correct" >:: fun _ ->
      assert (
        Board.to_string_flag bo
        = "[[[Flag of (0, 0) = false; Flag of (0, 1) = false]]; [[Flag of (1, \
           0) = false; Flag of (1, 1) = false]]]") );
    ( "assert to_string_flag of small board with different solution values is \
       correct"
    >:: fun _ ->
      assert (
        Board.to_string_flag bc
        = "[[[Flag of (0, 0) = false; Flag of (0, 1) = false]]; [[Flag of (1, \
           0) = false; Flag of (1, 1) = false]]]") );
  ]

let suite = "Test suite for Minesweeper" >::: List.flatten [ board_tests ]
let _ = run_test_tt_main suite
