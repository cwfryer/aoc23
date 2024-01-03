open Import

let year = 2023
let day = 12

let split_at n lst =
  let rec split_at' acc n lst =
    if n <= 0
    then Some (List.rev acc, lst)
    else (
      match lst with
      | [] -> None
      | hd :: tl -> split_at' (List.cons hd acc) (n - 1) tl)
  in
  split_at' [] n lst
;;

module Row = struct
  type t =
    { diagram : char list
    ; layout : int list
    }

  let solve diagram layout =
    let ht = Stdlib.Hashtbl.create 1024 in
    let rec solve' diagram layout =
      let res =
        match Stdlib.Hashtbl.find_opt ht (diagram, layout) with
        | Some x -> x
        | None ->
          (match diagram, layout with
           | [], [] -> 1
           | [], _ -> 0
           (* damaged springs remaining with no layouts remaining; invalid state *)
           | '#' :: _, [] -> 0
           (* normal spring encountered; layouts are not affected *)
           | '.' :: ss, _ -> solve' ss layout
           (* damaged spring encountered... *)
           | '#' :: _, r :: rs ->
             (* try to split off the first r elements of the list of springs *)
             (match split_at r diagram with
              | None -> 0
              | Some (prefix, next_nondamaged) ->
                (* if the next r springs are either damaged or  unknown, then they satisfy
                   the range criterion *)
                if List.for_all prefix ~f:(fun c ->
                     List.mem [ '#'; '?' ] c ~equal:Char.equal)
                then (
                  match next_nondamaged with
                  | '#' :: _ -> 0
                  (* the next spring cannot be damaged, so force it to be normal if ? *)
                  | '?' :: ss -> solve' ('.' :: ss) rs
                  | _ -> solve' next_nondamaged rs)
                else 0)
           (* ? spring encountered; count solutions for both alternate scenarios *)
           | '?' :: ss, _ -> solve' ('#' :: ss) layout + solve' ('.' :: ss) layout
           | _, _ -> failwith "found unexpected character")
      in
      Stdlib.Hashtbl.add ht (diagram, layout) res;
      res
    in
    solve' diagram layout
  ;;
end

module Machine = struct
  type t = Row.t list
end

module Angstrom = struct
  include Angstrom

  let diagram : char list t =
    let* diagram = many1 (char '.' <|> char '#' <|> char '?') in
    let* _ = char ' ' in
    (* let diagram = Array.of_list diagram in *)
    return diagram
  ;;

  let layout : int list t =
    let* layout = sep_by1 (char ',') unsigned_int in
    return layout
  ;;

  let row : Row.t t =
    let* diagram = diagram in
    let* layout = layout in
    let* _ = char '\n' in
    return Row.{ diagram; layout }
  ;;

  let machine : Machine.t t =
    let* rows = many1 row in
    return rows
  ;;
end

module Part_1 = struct
  let run (input : string) : (string, string) Result.t =
    let@ (machine : Machine.t) = Angstrom.(parse_string ~consume:All machine input) in
    let answer =
      List.map machine ~f:(fun row -> Row.solve row.diagram row.layout)
      |> List.fold ~init:0 ~f:( + )
      |> Int.to_string
    in
    Ok answer
  ;;
end

module Part_2 = struct
  let run (input : string) : (string, string) Result.t =
    let@ (machine : Machine.t) = Angstrom.(parse_string ~consume:All machine input) in
    let machine =
      List.map machine ~f:(fun row ->
        let diagram =
          row.diagram
          |> List.append [ '?' ]
          |> List.append row.diagram
          |> List.append [ '?' ]
          |> List.append row.diagram
          |> List.append [ '?' ]
          |> List.append row.diagram
          |> List.append [ '?' ]
          |> List.append row.diagram
        in
        let layout =
          row.layout
          |> List.append row.layout
          |> List.append row.layout
          |> List.append row.layout
          |> List.append row.layout
        in
        Row.{ diagram; layout })
    in
    let answer =
      List.map machine ~f:(fun row -> Row.solve row.diagram row.layout)
      |> List.fold ~init:0 ~f:( + )
      |> Int.to_string
    in
    Ok answer
  ;;
end
