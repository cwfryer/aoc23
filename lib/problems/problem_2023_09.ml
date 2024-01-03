open Import

let year = 2023
let day = 9

let find_next (l : Int.t List.t) : Int.t =
  let rec step (acc : Int.t List.t) (lst : Int.t List.t) =
    match List.for_all lst ~f:(fun el -> Int.equal el 0) with
    | true -> acc
    | false ->
      let last, deltas =
        List.fold lst ~init:(None, []) ~f:(fun (p, o) el ->
          match p with
          | None -> Some el, o
          | Some v -> Some el, (el - v) :: o)
      in
      let last = Option.value_exn last in
      step (last :: acc) (List.rev deltas)
  in
  let values = step [] l in
  List.fold values ~init:0 ~f:( + )
;;

module Part_1 = struct
  let run (input : string) : (string, string) Result.t =
    let answer =
      input
      |> String.split ~on:'\n'
      |> List.filter ~f:(fun line -> String.length line > 0)
      |> List.map ~f:(fun line ->
        List.map (String.split line ~on:' ') ~f:(fun el -> Int.of_string el))
      |> List.map ~f:find_next
      |> List.fold ~init:0 ~f:( + )
    in
    Ok (Int.to_string answer)
  ;;
end

module Part_2 = struct
  let run (input : string) : (string, string) Result.t =
    let answer =
      input
      |> String.split ~on:'\n'
      |> List.filter ~f:(fun line -> String.length line > 0)
      |> List.map ~f:(fun line ->
        List.map (String.split line ~on:' ') ~f:(fun el -> Int.of_string el))
      |> List.map ~f:List.rev
      |> List.map ~f:find_next
      |> List.fold ~init:0 ~f:( + )
    in
    Ok (Int.to_string answer)
  ;;
end
