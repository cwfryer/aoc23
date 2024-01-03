open Import

let year = 2023
let day = 16

module Grid = struct
  include Grid

  type beam =
    { coords : position
    ; direction : direction
    }

  type point =
    { coords : position
    ; shape : char
    ; energized : bool
    }

  type t = point Grid.t

  let of_string s : t =
    let grid =
      s
      |> String.split ~on:'\n'
      |> List.filter ~f:(fun line -> String.length line > 0)
      |> List.mapi ~f:(fun i line ->
        line
        |> String.to_array
        |> Array.mapi ~f:(fun j el -> { coords = j, i; shape = el; energized = false }))
      |> Array.of_list
    in
    grid
  ;;

  let print_grid t : unit =
    Stdio.printf "\nGrid after last step:\n";
    Array.iter t ~f:(fun line ->
      Array.iter line ~f:(fun p ->
        let c = if p.energized then '#' else p.shape in
        Stdio.printf "%c " c);
      Stdio.printf "\n")
  ;;

  let crawl (t : t) b : t =
    let rec run_beam_until_end beams t acc =
      match beams with
      | [] -> t
      | b :: bs ->
        if acc > 10
        then run_beam_until_end bs t 0
        else (
          let next = move b.direction b.coords in
          if inside t next
          then (
            let p = Grid.get t next in
            let acc' = if p.energized then acc + 1 else 0 in
            Grid.set t next { p with energized = true };
            let b' : beam list =
              match p.shape with
              | '|' ->
                (match b.direction with
                 | E | W ->
                   let b1 = { coords = next; direction = N } in
                   let b2 = { coords = next; direction = S } in
                   b1 :: b2 :: bs
                 | _ -> { b with coords = next } :: bs)
              | '-' ->
                (match b.direction with
                 | N | S ->
                   let b1 = { coords = next; direction = E } in
                   let b2 = { coords = next; direction = W } in
                   b1 :: b2 :: bs
                 | _ -> { b with coords = next } :: bs)
              | '\\' ->
                (match b.direction with
                 | N -> { coords = next; direction = W } :: bs
                 | S -> { coords = next; direction = E } :: bs
                 | E -> { coords = next; direction = S } :: bs
                 | W -> { coords = next; direction = N } :: bs
                 | _ -> failwith "Cannot travel diagonally")
              | '/' ->
                (match b.direction with
                 | N -> { coords = next; direction = E } :: bs
                 | S -> { coords = next; direction = W } :: bs
                 | E -> { coords = next; direction = N } :: bs
                 | W -> { coords = next; direction = S } :: bs
                 | _ -> failwith "Cannot travel diagonally")
              | _ -> { b with coords = next } :: bs
            in
            run_beam_until_end b' t acc')
          else run_beam_until_end bs t acc)
    in
    run_beam_until_end [ b ] t 0
  ;;

  let entry_points t : beam list =
    let left_side =
      List.init (Grid.height t) ~f:(fun r -> { coords = r, -1; direction = E })
    in
    let right_side =
      List.init (Grid.height t) ~f:(fun r -> { coords = r, Grid.width t; direction = W })
    in
    let top_side =
      List.init (Grid.width t) ~f:(fun c -> { coords = -1, c; direction = S })
    in
    let bot_side =
      List.init (Grid.width t) ~f:(fun c -> { coords = Grid.height t, c; direction = N })
    in
    List.concat [ left_side; right_side; top_side; bot_side ]
  ;;
end

module Part_1 = struct
  let run (input : string) : (string, string) Result.t =
    let grid = Grid.of_string input in
    let grid' = Grid.crawl grid { coords = 0, -1; direction = E } in
    let answer =
      Grid.fold
        (fun (_x, _y) (el : Grid.point) acc -> if el.energized then acc + 1 else acc)
        grid'
        0
      |> Int.to_string
    in
    Ok answer
  ;;
end

module Part_2 = struct
  let run (input : string) : (string, string) Result.t =
    let grid = Grid.of_string input in
    let entry_points = Grid.entry_points grid in
    let answer =
      List.fold entry_points ~init:0 ~f:(fun acc ep ->
        let grid = Grid.of_string input in
        let grid' = ep |> Grid.crawl grid in
        let v =
          Grid.fold
            (fun (_x, _y) (el : Grid.point) acc -> if el.energized then acc + 1 else acc)
            grid'
            0
        in
        if v > acc then v else acc)
    in
    Ok (Int.to_string answer)
  ;;
end
