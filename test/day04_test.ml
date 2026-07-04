open Core
open Aoc2025
open Runner.Make (Day04)

let input =
  {|
..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.|}
;;

let%expect_test "part 1" =
  run ~only_part1:true input;
  [%expect {| 13 |}]
;;

let%expect_test "part 2" =
  run ~only_part2:true input;
  [%expect {| 43 |}]
;;
