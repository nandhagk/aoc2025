open Core
open Aoc2025
open Runner.Make (Day03.M)

let input =
  {|
  987654321111111
  811111111111119
  234234234234278
  818181911112111
|}
;;

let%expect_test "part 1" =
  run ~only_part1:true input;
  [%expect {| 357 |}]
;;

let%expect_test "part 2" =
  run ~only_part2:true input;
  [%expect {| 3121910778619 |}]
;;
