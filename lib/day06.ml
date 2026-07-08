open Core

module Op = struct
  type t =
    | Add
    | Multiply
  [@@deriving sexp]

  let of_char_opt = function
    | '+' -> Some Add
    | '*' -> Some Multiply
    | _ -> None
  ;;

  let of_char c = c |> of_char_opt |> Option.value_exn

  let fold t list =
    match t with
    | Add -> List.fold list ~init:0 ~f:( + )
    | Multiply -> List.fold list ~init:1 ~f:( * )
  ;;
end

type t = (string list * Op.t) list [@@deriving sexp]

let chunk sizes list =
  let rec loop acc sizes list =
    match sizes with
    | [] -> acc
    | len :: sizes ->
      let hd, tl = List.split_n list len in
      loop (hd :: acc) sizes tl
  in
  list |> loop [] sizes |> List.rev
;;

let parse input =
  let input = input |> String.strip |> String.split_lines in
  let numbers, ops = List.drop_last_exn input, List.last_exn input in
  let width =
    numbers
    |> List.map ~f:(fun line ->
      line
      |> String.split ~on:' '
      |> List.filter ~f:(fun cell -> not (String.is_empty cell)))
    |> List.transpose_exn
    |> List.map ~f:(fun column ->
      column |> List.map ~f:String.length |> List.fold ~init:0 ~f:max)
  in
  let ops =
    ops
    |> String.split ~on:' '
    |> List.filter ~f:(fun cell -> not (String.is_empty cell))
    |> List.map ~f:(fun op -> op |> Char.of_string |> Op.of_char)
  in
  let numbers =
    numbers
    |> List.map ~f:(fun line ->
      line
      |> ( ^ ) " "
      |> String.to_list
      |> chunk (width |> List.map ~f:(( + ) 1))
      |> List.map ~f:(fun num -> num |> List.tl_exn))
    |> List.transpose_exn
    |> List.map2_exn width ~f:(fun len row ->
      List.map row ~f:(fun num -> num |> String.of_char_list |> String.pad_right ~len))
  in
  List.zip_exn numbers ops
;;

let part1 homework =
  homework
  |> List.map ~f:(fun (numbers, op) ->
    numbers |> List.map ~f:(fun num -> num |> String.strip |> Int.of_string) |> Op.fold op)
  |> List.fold ~init:0 ~f:( + )
  |> fun ans -> print_s [%sexp (ans : int)]
;;

let part2 homework =
  homework
  |> List.map ~f:(fun (numbers, op) ->
    numbers
    |> List.map ~f:String.to_list
    |> List.transpose_exn
    |> List.map ~f:(fun num ->
      num |> String.of_char_list |> String.strip |> Int.of_string)
    |> Op.fold op)
  |> List.fold ~init:0 ~f:( + )
  |> fun ans -> print_s [%sexp (ans : int)]
;;
