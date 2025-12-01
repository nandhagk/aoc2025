open Core

module type S = sig
  val run : ?only_part1:bool -> ?only_part2:bool -> string -> unit
end

module type Impl = sig
  type t

  val parse : string -> t
  val part1 : t -> unit
  val part2 : t -> unit
end

module Make (Impl : Impl) : S = struct
  let run ?(only_part1 = false) ?(only_part2 = false) input =
    let parsed = Impl.parse input in
    if not only_part2 then Impl.part1 parsed;
    if not only_part1 then Impl.part2 parsed;
    ()
  ;;
end
