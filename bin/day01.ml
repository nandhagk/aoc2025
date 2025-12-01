open Core
open Aoc2025
open Runner.Make (Day01.M)

let () = run (In_channel.read_all "./input/01.in")
