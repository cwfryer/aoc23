open Import

let year = 2023
let day = 13

module Grid = struct
  include Grid

  type t = char Grid.t

  let take_next n t = List.sub t ~pos:0 ~len:n

  let check_reflections (t : t) (smudges : int) =
    let rec check (acc : int list) i front back =
      match back with
      | [] -> acc
      | b :: bs ->
          let n = min (List.length front) (List.length back) in
          let front' = take_next n front in
          let back' = take_next n back in
          let acc' =
            let res =
              List.fold2_exn front' back' ~init:0 ~f:(fun count f b ->
                  let l =
                    Array.fold2_exn f b ~init:0 ~f:(fun acc f_c b_c ->
                        if not (Char.equal f_c b_c) then acc + 1 else acc)
                  in
                  count + l)
            in
            if Int.equal res smudges then i :: acc else acc
          in
          (check [@tailcall]) acc' (i + 1) (b :: front) bs
    in
    let t' = List.of_array t in
    let v_res =
      Option.value
        (List.max_elt ~compare:Int.compare
           ((check [@talcall]) [] 1 [ List.hd_exn t' ] (List.tl_exn t')))
        ~default:0
    in
    let t' = List.of_array (Grid.rotate_right t) in
    let h_res =
      Option.value
        (List.max_elt ~compare:Int.compare
           ((check [@talcall]) [] 1 [ List.hd_exn t' ] (List.tl_exn t')))
        ~default:0
    in
    (v_res, h_res)
end

module Valley = struct
  type t = Grid.t list
end

module Angstrom = struct
  include Angstrom

  let row : char array t =
    let* row = many1 (char '.' <|> char '#') in
    let* _ = char '\n' in
    let row = Array.of_list row in
    return row

  let grid : Grid.t t =
    let* rows = many1 row in
    let grid = Array.of_list rows in
    return grid

  let valley : Valley.t t =
    let* grids = sep_by1 (char '\n') grid in
    return grids
end

module Part_1 = struct
  let run (input : string) : (string, string) Result.t =
    let@ (valley : Valley.t) =
      Angstrom.(parse_string ~consume:All valley input)
    in
    let h_v =
      valley
      |> List.fold ~init:(0, 0) ~f:(fun (h, v) g ->
             let h', v' = Grid.check_reflections g 0 in
             (h + h', v + v'))
    in
    let answer = Int.to_string ((100 * fst h_v) + snd h_v) in
    Ok answer
end

module Part_2 = struct
  let run (input : string) : (string, string) Result.t =
    let@ (valley : Valley.t) =
      Angstrom.(parse_string ~consume:All valley input)
    in
    let h_v =
      valley
      |> List.fold ~init:(0, 0) ~f:(fun (h, v) g ->
             let h', v' = Grid.check_reflections g 1 in
             (h + h', v + v'))
    in
    let answer = Int.to_string ((100 * fst h_v) + snd h_v) in
    Ok answer
end
