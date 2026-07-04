module type S = sig
  val run : ?only_part1:bool -> ?only_part2:bool -> string -> unit
end

module Make : Solution.S -> S
