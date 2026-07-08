open Core
open Aoc2025
open Runner.Make (Day06)

let input =
  {|
123 328  51 64
 45 64  387 23
  6 98  215 314
*   +   *   +|}
;;

let%expect_test "part 1" =
  run ~only_part1:true input;
  [%expect {| 4277556 |}]
;;

let%expect_test "part 2" =
  run ~only_part2:true input;
  [%expect {| 3263827 |}]
;;
