open Core

module type S = sig
  val run : ?only_part1:bool -> ?only_part2:bool -> string -> unit
end

module Make (Solution : Solution.S) : S = struct
  let run ?(only_part1 = false) ?(only_part2 = false) input =
    let parsed = Solution.parse input in
    if not only_part2 then Solution.part1 parsed;
    if not only_part1 then Solution.part2 parsed
  ;;
end
