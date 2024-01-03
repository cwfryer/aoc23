let year = 2023
let day = 1

module Part_1 = struct
  let run (input : string) : (string, string) result =
    let numbers =
      input
      |> String.split_on_char '\n'
      |> List.filter (fun s -> String.length s > 0)
      |> List.map Core.String.to_list
      |> List.map (List.filter (fun c -> Core.Char.is_digit c))
      |> List.map (fun (xs : char list) ->
        match Core.List.(hd xs, last xs) with
        | None, None -> failwith "Empty list"
        | None, Some _ -> failwith "Illegal state"
        | Some f, None -> [ f; f ]
        | Some f, Some l -> [ f; l ])
      |> List.map Core.String.of_list
      |> List.map int_of_string
      |> Core.List.fold ~init:0 ~f:( + )
    in
    Ok (string_of_int @@ numbers)
  ;;
end

module Part_2 = struct
  let first_digit s =
    let rec aux chars =
      match chars with
      | 'o' :: 'n' :: 'e' :: _ -> '1'
      | 't' :: 'w' :: 'o' :: _ -> '2'
      | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: _ -> '3'
      | 'f' :: 'o' :: 'u' :: 'r' :: _ -> '4'
      | 'f' :: 'i' :: 'v' :: 'e' :: _ -> '5'
      | 's' :: 'i' :: 'x' :: _ -> '6'
      | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: _ -> '7'
      | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: _ -> '8'
      | 'n' :: 'i' :: 'n' :: 'e' :: _ -> '9'
      | c :: _ when Base.Char.is_digit c -> c
      | _ :: rest -> aux rest
      | [] -> '0'
    in
    aux (Base.String.to_list s)
  ;;

  let last_digit s =
    let rec aux chars =
      match chars with
      | 'e' :: 'n' :: 'o' :: _ -> '1'
      | 'o' :: 'w' :: 't' :: _ -> '2'
      | 'e' :: 'e' :: 'r' :: 'h' :: 't' :: _ -> '3'
      | 'r' :: 'u' :: 'o' :: 'f' :: _ -> '4'
      | 'e' :: 'v' :: 'i' :: 'f' :: _ -> '5'
      | 'x' :: 'i' :: 's' :: _ -> '6'
      | 'n' :: 'e' :: 'v' :: 'e' :: 's' :: _ -> '7'
      | 't' :: 'h' :: 'g' :: 'i' :: 'e' :: _ -> '8'
      | 'e' :: 'n' :: 'i' :: 'n' :: _ -> '9'
      | c :: _ when Base.Char.is_digit c -> c
      | _ :: rest -> aux rest
      | [] -> '0'
    in
    aux (Base.String.to_list (Base.String.rev s))
  ;;

  let run (input : string) : (string, string) result =
    let numbers =
      input
      |> String.split_on_char '\n'
      |> List.filter (fun s -> String.length s > 0)
      |> List.map (fun (s : string) -> [ first_digit s; last_digit s ])
      |> List.map Core.String.of_list
      |> List.map int_of_string
      |> Core.List.fold ~init:0 ~f:( + )
    in
    Ok (string_of_int @@ numbers)
  ;;
end
