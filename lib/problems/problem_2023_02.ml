open Base

let year = 2023
let day = 2

type set =
  { red : int
  ; green : int
  ; blue : int
  }

type game =
  { id : int
  ; sets : set List.t
  }

let set_color set color =
  let parts = String.split_on_chars ~on:[ ' ' ] color in
  let num = Int.of_string (List.hd_exn parts) in
  match List.nth_exn parts 1 with
  | "red" -> { set with red = set.red + num }
  | "blue" -> { set with blue = set.blue + num }
  | _ -> { set with green = set.green + num }
;;

let parse_set str =
  str
  |> Str.split (Str.regexp ", ")
  |> List.fold ~init:{ red = 0; green = 0; blue = 0 } ~f:set_color
;;

let parse_sets str = str |> Str.split (Str.regexp "; ") |> List.map ~f:parse_set
let game_r = Str.regexp {|Game \([0-9]+\): \(.*\)|}

let parse_game line =
  let _ = Str.string_match game_r line 0 in
  let id = Int.of_string (Str.matched_group 1 line) in
  let sets = parse_sets (Str.matched_group 2 line) in
  { id; sets }
;;

let games input =
  input
  |> String.strip
  |> String.split_on_chars ~on:[ '\n' ] (* lines *)
  |> List.map ~f:parse_game (* map line to game *)
;;

let is_possible values game =
  let set_possible x =
    values.red >= x.red && values.blue >= x.blue && values.green >= x.green
  in
  List.for_all ~f:set_possible game.sets
;;

let max_set a b =
  { red = max a.red b.red; blue = max a.blue b.blue; green = max a.green b.green }
;;

let power_of_game game =
  let maximums =
    List.fold game.sets ~init:{ red = 0; green = 0; blue = 0 } ~f:(fun acc s ->
      max_set acc s)
  in
  maximums.red * maximums.blue * maximums.green
;;

module Part_1 = struct
  let run (input : string) : (string, string) Result.t =
    let answer =
      games input
      |> List.filter ~f:(is_possible { red = 12; green = 13; blue = 14 })
      |> List.map ~f:(fun x -> x.id)
      |> List.fold ~init:0 ~f:( + )
    in
    Ok (Int.to_string answer)
  ;;
end

module Part_2 = struct
  let run (input : string) : (string, string) Result.t =
    let answer = games input |> List.map ~f:power_of_game |> List.fold ~init:0 ~f:( + ) in
    Ok (Int.to_string answer)
  ;;
end
