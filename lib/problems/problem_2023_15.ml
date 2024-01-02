open Import

let year = 2023
let day = 15

module Instruction = struct
  type operation = Add of int | Remove
  type t = { label : string; operation : operation }

  let of_string s : t =
    let rec go (label, instructions) chars =
      match chars with
      | [] -> (label, instructions)
      | c :: cs when Char.is_alpha c -> go (c :: label, instructions) cs
      | c :: _ when Char.equal c '-' -> go (label, Some Remove) []
      | c :: cs when Char.equal c '=' ->
          let n = cs |> List.hd_exn |> String.of_char |> Int.of_string in
          go (label, Some (Add n)) []
      | _ -> failwith "Invalid char in row"
    in
    let c, i = go ([], None) (String.to_list s) in
    match i with
    | Some operation ->
        let label = String.of_list (List.rev c) in
        { label; operation }
    | None -> failwith "Didn't find an instruction in row"

  let hash t =
    String.to_list t.label
    |> List.fold ~init:0 ~f:(fun acc c ->
           match c with
           | '\n' -> acc
           | _ ->
               let v = Char.to_int c in
               (acc + v) * 17 mod 256)

  let equal t t' = String.equal t.label t'.label
end

module Boxes = struct
  type t = Instruction.t array array

  let init : t = Array.init 256 ~f:(fun _ -> [||])

  let apply_instruction i t : unit =
    let box_num = Instruction.hash i in
    let box = Array.get t box_num in
    match i.operation with
    | Remove ->
        let contents =
          Array.filter box ~f:(fun mirror -> not (Instruction.equal i mirror))
        in
        Array.set t box_num contents
    | Add _ -> (
        match Array.findi box ~f:(fun _ el -> Instruction.equal i el) with
        | Some (n, _) -> Array.set box n i
        | None ->
            let contents = Array.append box [| i |] in
            Array.set t box_num contents)
end

module Part_1 = struct
  let run (input : string) : (string, string) Result.t =
    let answer =
      input |> String.split ~on:','
      |> List.map ~f:(fun row ->
             String.to_list row
             |> List.fold ~init:0 ~f:(fun acc c ->
                    match c with
                    | '\n' -> acc
                    | _ ->
                        let v = Char.to_int c in
                        (acc + v) * 17 mod 256))
      |> List.fold ~init:0 ~f:( + ) |> Int.to_string
    in
    Ok answer
end

module Part_2 = struct
  let run (input : string) : (string, string) Result.t =
    let instructions =
      input |> String.split ~on:',' |> List.map ~f:Instruction.of_string
    in
    let boxes = Boxes.init in
    let rec apply instructions boxes =
      match instructions with
      | [] -> boxes
      | i :: is ->
          Boxes.apply_instruction i boxes;
          apply is boxes
    in
    let boxes = apply instructions boxes in
    let answer =
      Array.foldi boxes ~init:0 ~f:(fun i acc box ->
          let v =
            Array.foldi box ~init:0
              ~f:(fun j acc' (instruction : Instruction.t) ->
                match instruction.operation with
                | Instruction.Remove -> acc'
                | Instruction.Add x -> acc' + ((1 + i) * (1 + j) * x))
          in
          acc + v)
      |> Int.to_string
    in
    Ok answer
end
