open OUnit2
open Game

(* The test plan should be located in a comment at the top of the test file.

   -4: The test plan is missing. -1: The test plan does not explain which parts
   of the system were automatically tested by OUnit vs. manually tested. -1: The
   test plan does not explain what modules were tested by OUnit and how test
   cases were developed (black box, glass box, randomized, etc.). -1: The test
   plan does not provide an argument for why the testing approach demonstrates
   the correctness of the system. *)
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
  |> Array.append (babyarraybomb x 3 5 false)
  |> Array.append (babyarraybomb x 2 5 false)
  |> Array.append (babyarraybomb x 1 5 false)
  |> Array.append (babyarraybomb x 0 3 false)

let middle x =
  (* babyarraybomb x 0 5 false |> Array.append (babyarraybomb x 1 8 false) |>
     Array.append (babyarraybomb x 2 8 false) |> Array.append (babyarraybomb x 3
     8 false) |> Array.append (babyarraybomb x 4 5 false) *)
  babyarraybomb x 4 5 false
  |> Array.append (babyarraybomb x 3 8 false)
  |> Array.append (babyarraybomb x 2 8 false)
  |> Array.append (babyarraybomb x 1 8 false)
  |> Array.append (babyarraybomb x 0 5 false)

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
  |> Array.append (Array.make 1 bline3)
  |> Array.append (Array.make 1 bline2)
  |> Array.append (Array.make 1 bline1)
  |> Array.append (Array.make 1 bline0)

(* testing count on a regatangular board *)
let toporbottomlarge x =
  babyarraybomb x 10 3 false
  |> Array.append (babyarraybomb x 9 5 false)
  |> Array.append (babyarraybomb x 8 5 false)
  |> Array.append (babyarraybomb x 7 5 false)
  |> Array.append (babyarraybomb x 6 5 false)
  |> Array.append (babyarraybomb x 5 5 false)
  |> Array.append (babyarraybomb x 4 5 false)
  |> Array.append (babyarraybomb x 3 5 false)
  |> Array.append (babyarraybomb x 2 5 false)
  |> Array.append (babyarraybomb x 1 5 false)
  |> Array.append (babyarraybomb x 0 3 false)

let middlelarge x =
  babyarraybomb x 10 5 false
  |> Array.append (babyarraybomb x 9 8 false)
  |> Array.append (babyarraybomb x 8 8 false)
  |> Array.append (babyarraybomb x 7 8 false)
  |> Array.append (babyarraybomb x 6 8 false)
  |> Array.append (babyarraybomb x 5 8 false)
  |> Array.append (babyarraybomb x 4 8 false)
  |> Array.append (babyarraybomb x 3 8 false)
  |> Array.append (babyarraybomb x 2 8 false)
  |> Array.append (babyarraybomb x 1 8 false)
  |> Array.append (babyarraybomb x 0 5 false)

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

let boardwithflag = boardwithbomblarge

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

let brd1 = boardcoord
let brd2 = boardcoord

let board_tests =
  [
    ( "test equals with same object" >:: fun _ ->
      assert (Board.isboardsequalquestionmarksimple emptyboard emptyboard) );
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
    ( "test count fails" >:: fun _ ->
      assert (
        flag 2 3 boardwithflag;
        Board.isboardsequalquestionmark boardwithbomb boardwithbomblarge
        == false) );
    ( "test get_flag no flag" >:: fun _ ->
      assert (Board.getflag boardwithflag 2 4 == false) );
    ( "test set_flag" >:: fun _ ->
      assert (
        flag 2 3 boardwithflag;
        Board.getflag boardwithflag 2 3 == true) );
    ( "test is_mine true" >:: fun _ ->
      assert (Board.ismine boardwithflag 2 3 == -1) );
    ("test is_mine false" >:: fun _ -> assert (Board.ismine boardcoord 2 3 == 0));
    ( "test random board generation" >:: fun _ ->
      assert (checkboardrandom 25 25 30) );
    ( "test random board always same for prob 100" >:: fun _ ->
      assert (checkboardrandom 25 25 100 == false) );
    ( "test random board always same for prob 0" >:: fun _ ->
      assert (checkboardrandom 25 25 0 == false) );
    ( "test no obs or sol when not called" >:: fun _ ->
      assert (ifobsthensolution (Board.newboard 11 5 100)) );
    ( "test no obs or sol when called with percentage >50" >:: fun _ ->
      assert (
        let brd = Board.newboard 11 10 85 in
        Board.placeobs brd;
        Board.placesol brd;
        ifobsthensolution brd) );
    ( "test no obs only when called with percentage >50" >:: fun _ ->
      assert (
        let brd = Board.newboard 11 10 90 in
        Board.placeobs brd;
        Board.placesol brd;
        obsinboard brd == false) );
    ( "test no sol only when called with percentage >50" >:: fun _ ->
      assert (
        let brd = Board.newboard 11 10 60 in
        Board.placeobs brd;
        Board.placesol brd;
        solinboard brd == false) );
    ( "test yes obs and sol when called with percentage <50" >:: fun _ ->
      assert (
        let brd = Board.newboard 11 10 20 in
        Board.placeobs brd;
        Board.placesol brd;
        ifobsthensolution brd) );
    ( "test yes obs only when called with percentage <50" >:: fun _ ->
      assert (
        let brd = Board.newboard 11 10 40 in
        Board.placeobs brd;
        Board.placesol brd;
        obsinboard brd) );
    ( "test yes sol only when called with percentage <50" >:: fun _ ->
      assert (
        let brd = Board.newboard 11 10 30 in
        Board.placeobs brd;
        Board.placesol brd;
        solinboard brd) );
    ( "assert creating obstacles with the same object twice will not result in \
       the same obstacle location"
    >:: fun _ ->
      assert (
        let _ = print_string "made it here! " in
        Board.placeobs brd1;
        Board.placesol brd1;
        Board.placeobs brd2;
        Board.placesol brd2;
        Board.isboardsequalquestionmark brd1 brd2 == false) )
    (* ( "assert creating obstacles with the same object twice will still result
       \ in the same simple grid" >:: fun _ -> assert ( let brd1 = boardcoord in
       Board.placeobs brd1; Board.placesol brd1; let brd2 = boardcoord in
       Board.placeobs brd2; Board.placesol brd2;
       Board.isboardsequalquestionmarksimple brd1 brd2) ); *);
  ]

let suite = "Test suite for Minesweeper" >::: List.flatten [ board_tests ]
let _ = run_test_tt_main suite
