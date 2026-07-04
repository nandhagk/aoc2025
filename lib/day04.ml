open Core

module Point = struct
  module T = struct
    type t = int * int [@@deriving compare, equal, sexp]
  end

  include T
  module Map = Map.Make (T)

  let deltas =
    let delta = [ -1; 0; 1 ] in
    List.cartesian_product delta delta
    |> List.filter ~f:(fun (dx, dy) -> not ([%equal: t] (dx, dy) (0, 0)))
  ;;

  let adjacent ((x, y) : t) : t list =
    deltas |> List.map ~f:(fun (dx, dy) -> x + dx, y + dy)
  ;;
end

module Cell = struct
  type t =
    | Roll
    | Empty
  [@@deriving sexp, variants]

  let of_char_opt = function
    | '@' -> Some Roll
    | '.' -> Some Empty
    | _ -> None
  ;;

  let of_char c = c |> of_char_opt |> Option.value_exn
end

type t = Cell.t Point.Map.t [@@deriving sexp]

let parse input =
  input
  |> String.strip
  |> String.split_lines
  |> List.concat_mapi ~f:(fun x row ->
    row |> String.to_list |> List.mapi ~f:(fun y cell -> (x, y), Cell.of_char cell))
  |> Point.Map.of_alist_exn
;;

let removable_rolls grid =
  grid
  |> Map.to_alist
  |> List.filter ~f:(fun (_, cell) -> Cell.is_roll cell)
  |> List.filter ~f:(fun (point, _) ->
    point
    |> Point.adjacent
    |> List.filter_map ~f:(Map.find grid)
    |> List.map ~f:(fun cell -> cell |> Cell.is_roll |> Bool.to_int)
    |> List.fold ~init:0 ~f:( + )
    |> ( > ) 4)
  |> List.map ~f:fst
;;

let part1 grid =
  grid |> removable_rolls |> List.length |> fun ans -> print_s [%sexp (ans : int)]
;;

let part2 grid =
  let rec loop grid =
    let rolls = grid |> removable_rolls in
    match rolls with
    | [] -> grid
    | _ -> loop (rolls |> List.fold ~init:grid ~f:Map.remove)
  in
  let initial, final =
    (grid, loop grid)
    |> Tuple2.map ~f:(fun grid ->
      grid |> Map.data |> List.filter ~f:Cell.is_roll |> List.length)
  in
  print_s [%sexp (initial - final : int)]
;;
