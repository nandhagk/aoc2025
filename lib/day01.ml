open Core

module Direction = struct
  type t =
    | Left
    | Right

  let of_char = function
    | 'L' -> Some Left
    | 'R' -> Some Right
    | _ -> None
  ;;
end

module Rotation = struct
  type t = Direction.t * int

  let pattern = Re2.create_exn {|([LR])(\d+)|}

  let of_string s =
    let open Option.Let_syntax in
    match Re2.find_submatches pattern s with
    | Ok [| _; Some direction; Some steps |] ->
      let direction = Char.of_string direction in
      let%bind direction = Direction.of_char direction in
      let%bind steps = Int.of_string_opt steps in
      Some (direction, steps)
    | _ -> None
  ;;

  let of_string_exn s = Option.value_exn (of_string s)
end

module Dial = struct
  type t = int

  let ticks = 100
  let make () : t = 50

  let rotate dial (direction, steps) : t =
    match direction with
    | Direction.Left -> (dial - steps) % ticks
    | Direction.Right -> (dial + steps) % ticks
  ;;

  let zeroes dial (direction, steps) : int =
    let first_zero =
      match direction with
      | Direction.Left -> dial
      | Direction.Right -> ticks - dial
    in
    let first_zero = if dial = 0 then ticks else first_zero in
    if steps < first_zero then 0 else 1 + ((steps - first_zero) / ticks)
  ;;
end

module M = struct
  type t = Rotation.t list

  let parse input =
    input |> String.strip |> String.split_lines |> List.map ~f:Rotation.of_string_exn
  ;;

  let part1 rotations =
    rotations
    |> List.folding_map ~init:(Dial.make ()) ~f:(fun dial rotation ->
      let dial = Dial.rotate dial rotation in
      dial, dial)
    |> List.count ~f:(( = ) 0)
    |> printf "%d\n"
  ;;

  let part2 rotations =
    rotations
    |> List.folding_map ~init:(Dial.make ()) ~f:(fun dial rotation ->
      Dial.rotate dial rotation, Dial.zeroes dial rotation)
    |> List.fold_left ~init:0 ~f:( + )
    |> printf "%d\n"
  ;;
end
