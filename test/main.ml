open OUnit2
open Game

let thisgameboard = ref (Board.newboard 6 5 15)
let thisboard = Array.make 5 (Board.tobox 0 0 0)

(* These value are used to test bombless boards *)
let emptybox = Board.tobox 0 0 0 0 false
let emptyline = Array.make 5 emptybox

(* let number (line : Board.box array) = Array.iteri (fun x boxb ->
   Board.changex boxb x ) *)
let emptyboard = Array.make 5 emptyline

(* A board with zero probability of a bomb *)
let zeroprobboard = Board.newboard 5 5 0

(* These values are used to test only bomb boards *)
let bombbox = Board.tobox 0 0 (-1) 0 false
let bombline = Array.make 5 bombbox
let bombboard = Array.make 5 bombline

(* A board with zero probability of a bomb *)
let hundredprobboard = Board.newboard 5 5 100

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
  ]

let suite = "Test suite for Minesweeper" >::: List.flatten [ board_tests ]
let _ = run_test_tt_main suite
