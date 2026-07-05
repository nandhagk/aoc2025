open Core

module Id = struct
  type t = int [@@deriving compare, sexp]

  let of_string_opt = Int.of_string_opt
  let of_string = Int.of_string
end

module Range = struct
  type t = Id.t * Id.t [@@deriving compare, sexp]

  let of_string_opt s =
    let%bind.Option lo, hi = String.lsplit2 ~on:'-' s in
    let%bind.Option lo = Id.of_string_opt lo in
    let%map.Option hi = Id.of_string_opt hi in
    lo, hi
  ;;

  let of_string s = s |> of_string_opt |> Option.value_exn
  let contains (lo, hi) ~id = lo <= id && id <= hi
end

type t = Range.t list * Id.t list

let merge (ranges : Range.t list) =
  match ranges with
  | [] -> []
  | top :: unmerged ->
    unmerged
    |> List.fold ~init:(top, []) ~f:(fun ((lo, hi), merged) (lo', hi') ->
      if lo' <= hi then (lo, max hi hi'), merged else (lo', hi'), (lo, hi) :: merged)
    |> Tuple2.uncurry List.cons
    |> List.rev
;;

let parse input =
  let fresh, available =
    input
    |> String.strip
    |> String.split_lines
    |> List.split_while ~f:(fun line -> not (String.is_empty line))
  in
  let fresh = fresh |> List.map ~f:Range.of_string in
  let available = available |> List.tl_exn |> List.map ~f:Id.of_string in
  fresh, available
;;

let part1 (fresh, available) =
  available
  |> List.filter_map ~f:(fun id -> List.find fresh ~f:(Range.contains ~id))
  |> List.length
  |> fun ans -> print_s [%sexp (ans : int)]
;;

let part2 (fresh, _) =
  fresh
  |> List.sort ~compare:Range.compare
  |> merge
  |> List.map ~f:(fun (lo, hi) -> hi - lo + 1)
  |> List.fold ~init:0 ~f:( + )
  |> fun ans -> print_s [%sexp (ans : int)]
;;
