open OUnit2
open Game

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
  babyarray x 0
  |> Array.append (babyarray x 1)
  |> Array.append (babyarray x 2)
  |> Array.append (babyarray x 3)
  |> Array.append (babyarray x 4)

let line1 = linex 0
let line2 = linex 1
let line3 = linex 2
let line4 = linex 3
let line5 = linex 4

let boardcoord =
  Array.make 1 line1
  |> Array.append (Array.make 1 line2)
  |> Array.append (Array.make 1 line3)
  |> Array.append (Array.make 1 line4)
  |> Array.append (Array.make 1 line5)

(* test bomb counting on a 100% board*)
let babyarraybomb x y count boop =
  Array.make 1 (Board.tobox x y (-1) count boop)

let toporbottom x =
  babyarraybomb x 0 3 false
  |> Array.append (babyarraybomb x 1 5 false)
  |> Array.append (babyarraybomb x 2 5 false)
  |> Array.append (babyarraybomb x 3 5 false)
  |> Array.append (babyarraybomb x 4 3 false)

let middle x =
  babyarraybomb x 0 5 false
  |> Array.append (babyarraybomb x 1 8 false)
  |> Array.append (babyarraybomb x 2 8 false)
  |> Array.append (babyarraybomb x 3 8 false)
  |> Array.append (babyarraybomb x 4 5 false)

let bline0 = toporbottom 0
let bline1 = middle 1
let bline2 = middle 2
let bline3 = middle 3
let bline4 = toporbottom 4

let boardwithbomb =
  Array.make 1 bline0
  |> Array.append (Array.make 1 bline1)
  |> Array.append (Array.make 1 bline2)
  |> Array.append (Array.make 1 bline3)
  |> Array.append (Array.make 1 bline4)

(* testing count on a regatangular board *)
let toporbottomlarge x =
  babyarraybomb x 0 3 false
  |> Array.append (babyarraybomb x 1 5 false)
  |> Array.append (babyarraybomb x 2 5 false)
  |> Array.append (babyarraybomb x 3 5 false)
  |> Array.append (babyarraybomb x 4 5 false)
  |> Array.append (babyarraybomb x 5 5 false)
  |> Array.append (babyarraybomb x 6 5 false)
  |> Array.append (babyarraybomb x 7 5 false)
  |> Array.append (babyarraybomb x 8 5 false)
  |> Array.append (babyarraybomb x 9 5 false)
  |> Array.append (babyarraybomb x 10 3 false)

let middlelarge x =
  babyarraybomb x 0 5 false
  |> Array.append (babyarraybomb x 1 8 false)
  |> Array.append (babyarraybomb x 2 8 false)
  |> Array.append (babyarraybomb x 3 8 false)
  |> Array.append (babyarraybomb x 4 8 false)
  |> Array.append (babyarraybomb x 5 8 false)
  |> Array.append (babyarraybomb x 6 8 false)
  |> Array.append (babyarraybomb x 7 8 false)
  |> Array.append (babyarraybomb x 8 8 false)
  |> Array.append (babyarraybomb x 9 8 false)
  |> Array.append (babyarraybomb x 10 5 false)

let blinelarge0 = toporbottomlarge 0
let blinelarge1 = middlelarge 1
let blinelarge2 = middlelarge 2
let blinelarge3 = middlelarge 3
let blinelarge4 = toporbottomlarge 4

let boardwithbomblarge =
  Array.make 1 blinelarge0
  |> Array.append (Array.make 1 blinelarge1)
  |> Array.append (Array.make 1 blinelarge2)
  |> Array.append (Array.make 1 blinelarge3)
  |> Array.append (Array.make 1 blinelarge4)

let boardwithflag = boardwithbomblarge

(* Set position x y as flagged *)
let flag x y board = Board.setflag board x y

let board_tests =
  [
    ( "test equals with same object" >:: fun _ ->
      assert (Board.isboardsequalquestionmark emptyboard emptyboard) );
    ( "test equals with different object" >:: fun _ ->
      assert (Board.isboardsequalquestionmark emptyboard zeroprobboard) );
    ( "test equals with non square boards" >:: fun _ ->
      assert (
        Board.isboardsequalquestionmark (Array.make 6 emptyline)
          (Board.newboard 5 6 0)) );
    ( "test equals with different bombfull object" >:: fun _ ->
      assert (Board.isboardsequalquestionmark bombboard hundredprobboard) );
    ( "test equals with non square boards" >:: fun _ ->
      assert (
        Board.isboardsequalquestionmark (Array.make 6 bombline)
          (Board.newboard 5 6 100)) );
    ( "test equals with coord boards" >:: fun _ ->
      assert (Board.isboardsequalquestionmark boardcoord (Board.newboard 5 5 0))
    );
    ( "test count" >:: fun _ ->
      assert (
        Board.isboardsequalquestionmark boardwithbomb (Board.newboard 5 5 100))
    );
    ( "test count on a rectangular boards" >:: fun _ ->
      assert (
        Board.isboardsequalquestionmark boardwithbomblarge
          (Board.newboard 10 5 100)) );
    ( "test get_flag no flag" >:: fun _ ->
      assert (Board.getflag boardwithbomblarge 2 3 == false) );
    ( "test set_flag" >:: fun _ ->
      assert (
        flag 2 3 boardwithflag;
        Board.getflag boardwithflag 2 3 == true) );
  ]

let suite = "Test suite for Minesweeper" >::: List.flatten [ board_tests ]
let _ = run_test_tt_main suite
