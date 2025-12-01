open Core
open Aoc2025
open Runner.Make (Day01.M)

let input =
  {|
  L68
  L30
  R48
  L5
  R60
  L55
  L1
  L99
  R14
  L82
|}
;;

let%expect_test "part 1" =
  run ~only_part1:true input;
  [%expect {| 3 |}]
;;

let%expect_test "part 2" =
  run ~only_part2:true input;
  [%expect {| 6 |}]
;;
