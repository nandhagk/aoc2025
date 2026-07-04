open Core

module Bank = struct
  type t = string

  let of_string s : t = s

  (* https://www.geeksforgeeks.org/dsa/lexicographically-smallest-k-length-subsequence-from-a-given-string/ *)
  let smallest_subseq ~k ~cmp xs =
    let len = List.length xs in
    let pop_while x stk len rem =
      let rec aux stk len =
        match stk with
        | top :: btm when cmp top x > 0 && len - 1 + rem >= k -> aux btm (len - 1)
        | _ -> stk, len
      in
      aux stk len
    in
    xs
    |> List.fold_left ~init:([], 0, len) ~f:(fun (stk, len, rem) x ->
      let stk, len = pop_while x stk len rem in
      if len < k then x :: stk, len + 1, rem - 1 else stk, len, rem - 1)
    |> (fun (stk, _, _) -> stk)
    |> List.rev
  ;;

  let max_joltage bank ~k =
    bank
    |> String.to_list
    |> smallest_subseq ~k ~cmp:(fun a b -> Char.compare b a)
    |> String.of_char_list
    |> Int.of_string
  ;;
end

type t = Bank.t list

let parse input =
  input |> String.strip |> String.split_lines |> List.map ~f:Bank.of_string
;;

let part1 banks =
  banks
  |> List.map ~f:(Bank.max_joltage ~k:2)
  |> List.fold ~init:0 ~f:( + )
  |> fun ans -> print_s [%sexp (ans : int)]
;;

let part2 banks =
  banks
  |> List.map ~f:(Bank.max_joltage ~k:12)
  |> List.fold ~init:0 ~f:( + )
  |> fun ans -> print_s [%sexp (ans : int)]
;;
