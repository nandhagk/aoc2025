open Core

module Day = struct
  type t =
    | Day01
    | Day02
    | Day03
    | Day04
  [@@deriving string ~capitalize:"snake_case", sexp, enumerate]

  let arg_type = Command.Arg_type.create of_string

  let to_solution : t -> (module Aoc2025.Solution.S) = function
    | Day01 -> (module Aoc2025.Day01)
    | Day02 -> (module Aoc2025.Day02)
    | Day03 -> (module Aoc2025.Day03)
    | Day04 -> (module Aoc2025.Day04)
  ;;
end

let command =
  Command.basic
    ~summary:"AoC 2025"
    (let%map_open.Command day =
       flag "-day" (required Day.arg_type) ~doc:"DAY day to run solution of"
     in
     fun () ->
       let module Solution = (val Day.to_solution day) in
       let module Runner = Aoc2025.Runner.Make (Solution) in
       let path = [%string "./input/%{day#Day}.in"] in
       Runner.run (In_channel.read_all path))
;;

let () = Command_unix.run command
