open Core
open Aoc2025
open Runner.Make (Day05)

let input =
  {|
3-5
10-14
16-20
12-18

1
5
8
11
17
32|}
;;

let%expect_test "part 1" =
  run ~only_part1:true input;
  [%expect {| 3 |}]
;;

let%expect_test "part 2" =
  run ~only_part2:true input;
  [%expect {| 14 |}]
;;
