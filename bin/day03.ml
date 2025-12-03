open Core
open Aoc2025
open Runner.Make (Day03.M)

let () = run (In_channel.read_all "./input/03.in")
