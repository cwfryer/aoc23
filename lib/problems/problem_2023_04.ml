open Base

let year = 2023
let day = 4

module Part_1 = struct
  let run (input : string) : (string, string) Result.t =
    let scores =
      input
      |> String.split_on_chars ~on:[ '\n' ]
      |> List.filter ~f:(fun s -> String.length s > 0)
      |> List.map ~f:(fun line ->
             let parts =
               String.split_on_chars line ~on:[ ':'; '|' ] |> Array.of_list
             in
             let wins =
               parts.(1) |> String.split ~on:' '
               |> List.filter_map ~f:(fun n ->
                      if Int.equal (String.length n) 0 then None
                      else Some (Int.of_string n))
             in
             let have =
               parts.(2) |> String.split ~on:' '
               |> List.filter_map ~f:(fun n ->
                      if Int.equal (String.length n) 0 then None
                      else Some (Int.of_string n))
             in
             let count =
               List.fold wins ~init:0 ~f:(fun acc n ->
                   if List.mem have n ~equal:Int.equal then acc + 1 else acc)
             in
             if Int.equal count 0 then 0 else Int.pow 2 (count - 1))
    in
    Ok (Int.to_string (List.fold scores ~init:0 ~f:( + )))
end

module Part_2 = struct
  let run (input : string) : (string, string) Result.t =
    let scores =
      let lines =
        input
        |> String.split_on_chars ~on:[ '\n' ]
        |> List.filter ~f:(fun s -> String.length s > 0)
      in
      List.foldi lines
        ~init:(Array.init (List.length lines) ~f:(fun _ -> 0))
        ~f:(fun i win_array line ->
          win_array.(i) <- win_array.(i) + 1;
          (* add 1 of the current card *)
          let parts =
            String.split_on_chars line ~on:[ ':'; '|' ] |> Array.of_list
          in
          let wins =
            parts.(1) |> String.split ~on:' '
            |> List.filter_map ~f:(fun n ->
                   if Int.equal (String.length n) 0 then None
                   else Some (Int.of_string n))
          in
          let have =
            parts.(2) |> String.split ~on:' '
            |> List.filter_map ~f:(fun n ->
                   if Int.equal (String.length n) 0 then None
                   else Some (Int.of_string n))
          in
          let count =
            (* where count is the number of subsequent cards that I get bonuses of *)
            List.fold wins ~init:0 ~f:(fun acc n ->
                if List.mem have n ~equal:Int.equal then acc + 1 else acc)
          in
          let rec update_win_array i j =
            match j with
            | 0 -> () (* at 0 you're done, move on *)
            | _ ->
                (* add the number of cards we have to the cards that pay out *)
                win_array.(i + j) <- win_array.(i + j) + win_array.(i);
                (* go back up on card and continue *)
                update_win_array i (j - 1)
          in
          update_win_array i count;
          win_array)
      |> Array.fold ~init:0 ~f:( + )
    in
    Ok (Int.to_string scores)
end
