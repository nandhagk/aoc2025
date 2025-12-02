open Core
open Aoc2025
open Runner.Make (Day02.M)

let () = run (In_channel.read_all "./input/02.in")
