module type S = sig
  type t

  val parse : string -> t
  val part1 : t -> unit
  val part2 : t -> unit
end
