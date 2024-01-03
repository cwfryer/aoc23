open Import

let year = 2023
let day = 7

module Hands = struct
  type hand =
    { cards : char list
    ; bid : int
    }

  type t = hand list

  let score_hand part (h : hand) : Float.t =
    let cards = h.cards in
    let freq_count =
      List.fold cards ~init:[] ~f:(fun counts c ->
        let count =
          match List.Assoc.find ~equal:Char.equal counts c with
          | None -> 0
          | Some x -> x
        in
        List.Assoc.add ~equal:Char.equal counts c (count + 1))
      |> List.sort ~compare:(fun (_, x) (_, y) -> Int.descending x y)
    in
    let jokers =
      if part then List.Assoc.find freq_count ~equal:Char.equal 'J' else None
    in
    let freq_count =
      if part
      then List.filter freq_count ~f:(fun (c, _) -> not (phys_equal c 'J'))
      else freq_count
    in
    let v, _ =
      match jokers with
      | Some x ->
        if List.length freq_count > 1
        then
          List.fold freq_count ~init:(0., x) ~f:(fun (acc, j) (_, c) ->
            match c with
            | 4 -> 6., 0
            | 3 -> if j > 1 then 6., 0 else 5., 0
            | 2 ->
              (match j with
               | 3 -> 6., 0
               | 2 -> 5., 0
               | 1 -> 3., 0
               | _ -> acc +. 1., 0)
            | _ ->
              (match j with
               | 4 -> 6., 0
               | 3 -> 5., 0
               | 2 -> 3., 0
               | 1 -> 1., 0
               | _ -> acc, 0))
        else 6., 0
      | None ->
        if List.length freq_count > 1
        then
          List.fold freq_count ~init:(0., 0) ~f:(fun (acc, _) (_, c) ->
            match c with
            | 4 -> 5., 0
            | 3 -> 3., 0
            | 2 -> acc +. 1., 0
            | _ -> acc, 0)
        else 6., 0
    in
    v
  ;;

  let sort part : t -> t =
    List.sort ~compare:(fun h1 h2 ->
      let cards_to_val (h : hand) =
        let s =
          "0."
          ^ List.fold h.cards ~init:{||} ~f:(fun acc c ->
            let v =
              match c with
              | 'A' -> "14"
              | 'K' -> "13"
              | 'Q' -> "12"
              | 'J' -> if part then "01" else "11"
              | 'T' -> "10"
              | '9' -> "09"
              | '8' -> "08"
              | '7' -> "07"
              | '6' -> "06"
              | '5' -> "05"
              | '4' -> "04"
              | '3' -> "03"
              | _ -> "02"
            in
            acc ^ v)
        in
        Float.of_string s
      in
      let v1 = score_hand part h1 +. cards_to_val h1 in
      let v2 = score_hand part h2 +. cards_to_val h2 in
      Float.compare v1 v2)
  ;;
end

module Angstrom = struct
  include Angstrom

  let map_row =
    let* cards =
      take_while1
      @@ function
      | '0' .. '9' -> true
      | 'A' .. 'Z' -> true
      | _ -> false
    in
    let* _ = char ' ' in
    let* bid = unsigned_int in
    let* _ = char '\n' in
    return Hands.{ cards = String.to_list cards; bid }
  ;;

  let hands : Hands.t t =
    let* hands = many1 map_row in
    return hands
  ;;
end

module Part_1 = struct
  let run (input : string) : (string, string) Result.t =
    let@ hands = Angstrom.(parse_string ~consume:All hands input) in
    let sorted_hands = Hands.sort false hands in
    let payout =
      List.foldi sorted_hands ~init:0 ~f:(fun i acc h -> acc + ((i + 1) * h.bid))
    in
    Ok (Int.to_string payout)
  ;;
end

module Part_2 = struct
  let run (input : string) : (string, string) Result.t =
    let@ hands = Angstrom.(parse_string ~consume:All hands input) in
    let sorted_hands = Hands.sort true hands in
    let payout =
      List.foldi sorted_hands ~init:0 ~f:(fun i acc h -> acc + ((i + 1) * h.bid))
    in
    Ok (Int.to_string payout)
  ;;
end
