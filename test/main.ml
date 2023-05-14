open OUnit2
open Game

let thisgameboard = ref (Board.newboard 6 5 15)
let thisboard = Array.make 5 (Board.tobox 0 0 0)
let emptybox = Board.tobox 0 0 0
let emptyline = Array.make 5 emptybox
let emptyboard = Array.make 5 emptyline

let board_tests =
  [
    ( "empty board test" >:: fun _ ->
      assert (Board.isboardsequalquestionmark emptyboard emptyboard) );
  ]

let suite = "Test suite for Minesweeper" >::: List.flatten [ board_tests ]
let _ = run_test_tt_main suite
