open Core
open Aoc2025
open Runner.Make (Day02)

let input =
  {|11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124|}
;;

let%expect_test "part 1" =
  run ~only_part1:true input;
  [%expect {| 1227775554 |}]
;;

let%expect_test "part 2" =
  run ~only_part2:true input;
  [%expect {| 4174379265 |}]
;;
