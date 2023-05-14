open OUnit2
open Game

let board_tests = []
let suite = "Test suite for Minesweeper" >::: List.flatten [ board_tests ]
let _ = run_test_tt_main suite s
