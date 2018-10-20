open OUnit2

let suite = "test suite"  >::: List.flatten [
]

let _ = run_test_tt_main suite
