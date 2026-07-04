open Core

module Range = struct
  type t = int * int

  let create lo hi = lo, hi

  let of_string_opt s =
    match String.split ~on:'-' s with
    | [ lo; hi ] ->
      let%bind.Option lo = Int.of_string_opt lo in
      let%map.Option hi = Int.of_string_opt hi in
      create lo hi
    | _ -> None
  ;;

  let of_string s = s |> of_string_opt |> Option.value_exn
end

type t = Range.t list

let is_invalid ?(repeat = 2) s =
  let len = String.length s / repeat in
  let t = String.sub s ~pos:0 ~len in
  let t = Fn.apply_n_times ~n:repeat (( ^ ) t) "" in
  String.equal s t
;;

let parse input =
  input |> String.strip |> String.split ~on:',' |> List.map ~f:Range.of_string
;;

let part1 ranges =
  ranges
  |> List.map ~f:(fun (lo, hi) ->
    hi - lo + 1
    |> List.init ~f:(fun i -> i + lo)
    |> List.filter ~f:(fun x -> x |> Int.to_string |> is_invalid))
  |> List.concat
  |> List.fold ~init:0 ~f:( + )
  |> fun ans -> print_s [%sexp (ans : int)]
;;

let part2 ranges =
  ranges
  |> List.map ~f:(fun (lo, hi) ->
    hi - lo + 1
    |> List.init ~f:(fun i -> i + lo)
    |> List.filter ~f:(fun x ->
      let s = Int.to_string x in
      List.init (String.length s - 1) ~f:(fun i -> i + 2)
      |> List.map ~f:(fun repeat -> is_invalid s ~repeat)
      |> List.fold ~f:( || ) ~init:false))
  |> List.concat
  |> List.fold ~init:0 ~f:( + )
  |> fun ans -> print_s [%sexp (ans : int)]
;;
