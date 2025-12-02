open Core

module Range = struct
  type t = int * int

  let make lo hi = lo, hi

  let of_string s =
    let open Option.Let_syntax in
    match String.split ~on:'-' s with
    | [ lo; hi ] ->
      let%bind lo = Int.of_string_opt lo in
      let%bind hi = Int.of_string_opt hi in
      Some (make lo hi)
    | _ -> None
  ;;

  let of_string_exn s = Option.value_exn (of_string s)
end

module M = struct
  type t = Range.t list

  let is_invalid ?(repeat = 2) s =
    let len = String.length s / repeat in
    let t = String.sub s ~pos:0 ~len in
    let t = Fn.apply_n_times ~n:repeat (( ^ ) t) "" in
    String.equal s t
  ;;

  let parse input =
    input |> String.strip |> String.split ~on:',' |> List.map ~f:Range.of_string_exn
  ;;

  let part1 ranges =
    ranges
    |> List.map ~f:(fun (lo, hi) ->
      List.init (hi - lo + 1) ~f:(fun i -> i + lo)
      |> List.filter ~f:(fun x ->
        let s = Int.to_string x in
        is_invalid s))
    |> List.concat
    |> List.fold ~f:( + ) ~init:0
    |> printf "%d\n"
  ;;

  let part2 ranges =
    ranges
    |> List.map ~f:(fun (lo, hi) ->
      List.init (hi - lo + 1) ~f:(fun i -> i + lo)
      |> List.filter ~f:(fun x ->
        let s = Int.to_string x in
        List.init (String.length s - 1) ~f:(fun i -> i + 2)
        |> List.map ~f:(fun repeat -> is_invalid s ~repeat)
        |> List.fold ~f:( || ) ~init:false))
    |> List.concat
    |> List.fold ~f:( + ) ~init:0
    |> printf "%d\n"
  ;;
end
