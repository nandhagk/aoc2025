open Core

module Direction = struct
  type t =
    | Left
    | Right

  let of_char_opt = function
    | 'L' -> Some Left
    | 'R' -> Some Right
    | _ -> None
  ;;
end

module Rotation = struct
  type t = Direction.t * int

  let pattern = Re2.create_exn {|([LR])(\d+)|}

  let of_string_opt s =
    match Re2.find_submatches pattern s with
    | Ok [| _; Some direction; Some steps |] ->
      let direction = Char.of_string direction in
      let%bind.Option direction = Direction.of_char_opt direction in
      let%map.Option steps = Int.of_string_opt steps in
      direction, steps
    | _ -> None
  ;;

  let of_string s = s |> of_string_opt |> Option.value_exn
end

module Dial = struct
  type t = int

  let ticks = 100
  let create () : t = 50

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

type t = Rotation.t list

let parse input =
  input |> String.strip |> String.split_lines |> List.map ~f:Rotation.of_string
;;

let part1 rotations =
  rotations
  |> List.folding_map ~init:(Dial.create ()) ~f:(fun dial rotation ->
    let dial = Dial.rotate dial rotation in
    dial, dial)
  |> List.count ~f:(( = ) 0)
  |> fun ans -> print_s [%sexp (ans : int)]
;;

let part2 rotations =
  rotations
  |> List.folding_map ~init:(Dial.create ()) ~f:(fun dial rotation ->
    Dial.rotate dial rotation, Dial.zeroes dial rotation)
  |> List.fold_left ~init:0 ~f:( + )
  |> fun ans -> print_s [%sexp (ans : int)]
;;
