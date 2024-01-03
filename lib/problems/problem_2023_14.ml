open Import

let year = 2023
let day = 14

type space =
  | Rock
  | Wall
  | Empty

module Grid = struct
  include Grid

  type t = space Grid.t

  let print_grid t =
    Stdio.printf "\n";
    Array.iter t ~f:(fun row ->
      Array.iter row ~f:(fun p ->
        let c =
          match p with
          | Rock -> 'O'
          | Wall -> '#'
          | Empty -> '.'
        in
        Stdio.printf "%c" c);
      Stdio.printf "\n")
  ;;

  let from_string s : t =
    s
    |> String.split ~on:'\n'
    |> List.filter ~f:(fun row -> String.length row > 0)
    |> List.map ~f:(fun row ->
      let a = String.to_array row in
      Array.map a ~f:(fun c ->
        match c with
        | 'O' -> Rock
        | '#' -> Wall
        | _ -> Empty))
    |> Array.of_list
  ;;

  let tilt (t : t) (dir : direction) : t =
    let n = max (height t) (width t) in
    let res = t in
    for _ = 0 to n do
      (* Stdio.printf "\n"; *)
      (* print_grid res; *)
      iter
        (fun (x, y) s ->
          match s with
          | Rock ->
            let p = move dir (x, y) in
            if inside t p
            then (
              match get t p with
              | Rock -> ()
              | Wall -> ()
              | Empty ->
                set t p Rock;
                set t (x, y) Empty)
            else ()
          | Wall -> ()
          | Empty -> ())
        res
    done;
    res
  ;;

  let spin_cycle t : t =
    let a = tilt t N in
    let b = tilt a W in
    let c = tilt b S in
    tilt c E
  ;;

  let spin_until_equilibrium t : t =
    let rec spin_again n t : t =
      let spin = spin_cycle t in
      if Int.equal n 0 then t else spin_again (n - 1) spin
    in
    (* by coincidence 1,000 cycles works *)
    spin_again 999 t
  ;;

  let calculate_load t : int =
    let rev = Array.rev t in
    Array.foldi rev ~init:0 ~f:(fun i acc row ->
      let cnt =
        Array.fold row ~init:0 ~f:(fun acc c ->
          match c with
          | Rock -> acc + 1
          | _ -> acc)
      in
      acc + (cnt * (i + 1)))
  ;;
end

module Part_1 = struct
  let run (input : string) : (string, string) Result.t =
    let grid = Grid.from_string input in
    let output = Grid.tilt grid N in
    let answer = output |> Grid.calculate_load |> Int.to_string in
    Ok answer
  ;;
end

module Part_2 = struct
  let run (input : string) : (string, string) Result.t =
    let grid = Grid.from_string input in
    let output = Grid.spin_until_equilibrium grid in
    let answer = output |> Grid.calculate_load |> Int.to_string in
    Ok answer
  ;;
end
