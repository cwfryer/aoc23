open Import

let year = 2023
let day = 11

module Graph = struct
  include Grid

  type t = char Grid.t

  let of_string s =
    s |> String.split ~on:'\n'
    |> List.filter ~f:(fun row -> String.length row > 0)
    |> Array.of_list
    |> Array.map ~f:(fun row -> String.to_array row)

  let find_wormholes t =
    let explore_one_dimension t (acc : int list * int list) =
      Array.foldi t ~init:acc ~f:(fun i (xs, ys) line ->
          if Array.for_all line ~f:(fun el -> Char.equal el '.') then
            (i :: xs, ys)
          else (xs, ys))
    in
    let explore_other_dimension t (acc : int list * int list) =
      let t = rotate_right t in
      Array.foldi t ~init:acc ~f:(fun i (xs, ys) line ->
          if Array.for_all line ~f:(fun el -> Char.equal el '.') then
            (xs, i :: ys)
          else (xs, ys))
    in
    let one = explore_one_dimension t ([], []) in
    explore_other_dimension t one

  let expand t =
    let expand_one_dimension t =
      Array.fold t ~init:[||] ~f:(fun acc line ->
          if Array.for_all line ~f:(fun el -> Char.equal el '.') then
            Array.append acc [| line; line |]
          else Array.append acc [| line |])
    in
    let expand_other_dimension t =
      rotate_right t |> expand_one_dimension |> rotate_left
    in
    t |> expand_one_dimension |> expand_other_dimension

  let galaxies t =
    Array.foldi t ~init:[] ~f:(fun i acc row ->
        Array.foldi ~init:acc row ~f:(fun j acc el ->
            if Char.equal el '#' then (i, j) :: acc else acc))

  let expand_by_n (x_jumps, y_jumps) n galaxies =
    List.map galaxies ~f:(fun galaxy ->
        let incr_x_by =
          List.fold x_jumps ~init:0 ~f:(fun acc jump ->
              let x = fst galaxy in
              if jump < x then acc + 1 else acc)
        in
        let incr_y_by =
          List.fold y_jumps ~init:0 ~f:(fun acc jump ->
              let y = snd galaxy in
              if jump < y then acc + 1 else acc)
        in
        (fst galaxy + ((n - 1) * incr_x_by), snd galaxy + ((n - 1) * incr_y_by)))

  let galaxy_pairs galaxies : ((int * int) * (int * int)) list =
    let rec go (lst : (int * int) list) (acc : ((int * int) * (int * int)) list)
        =
      match lst with
      | [] -> acc
      | x :: xs ->
          let acc = List.append acc (List.map xs ~f:(fun p -> (x, p))) in
          go xs acc
    in
    go galaxies []
end

let manhattan x1 x2 y1 y2 = abs (x1 - x2) + abs (y1 - y2)

module Part_1 = struct
  let run (input : string) : (string, string) Result.t =
    let answer =
      input |> Graph.of_string |> Graph.expand |> Graph.galaxies
      |> Graph.galaxy_pairs
      |> List.map ~f:(fun ((x1, y1), (x2, y2)) -> manhattan x1 x2 y1 y2)
      |> List.fold ~init:0 ~f:( + ) |> Int.to_string
    in
    Ok answer
end

module Part_2 = struct
  let run (input : string) : (string, string) Result.t =
    let graph = Graph.of_string input in
    let wormholes = Graph.find_wormholes graph in
    let answer =
      graph |> Graph.galaxies
      |> Graph.expand_by_n wormholes 1000000
      |> Graph.galaxy_pairs
      |> List.map ~f:(fun ((x1, y1), (x2, y2)) -> manhattan x1 x2 y1 y2)
      |> List.fold ~init:0 ~f:( + ) |> Int.to_string
    in
    Ok answer
end
