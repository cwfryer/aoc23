open Import

let year = 2023
let day = 20

(* type definitions *)
type operation =
  | Broadcast
  | FlipFlop of bool
  | Conjunction of (string, bool) Hashtbl.t

module Mod = struct
  type t =
    { label : string
    ; operation : operation
    ; destinations : string list
    }

  let print t : unit =
    let c =
      match t.operation with
      | Broadcast -> ""
      | FlipFlop _ -> "% "
      | Conjunction _ -> "& "
    in
    Stdio.printf "%s%s -> " c t.label;
    List.iter t.destinations ~f:(fun d -> Stdio.printf "%s " d);
    Stdio.printf "\n"
  ;;

  let process_operation (sender : string) pulse t : t * (string * bool * string) list =
    (* Stdio.printf "\n---------------------\nprocessing operation\n- - - - - - - - - - -\n"; *)
    (* Stdio.printf *)
    (*   "sent by %s, operation '%s' received a %s pulse\n" *)
    (*   sender *)
    (*   t.label *)
    (*   (if pulse then "high" else "low"); *)
    match t.operation with
    | Broadcast ->
      (* Stdio.printf "%s is sending low to " t.label; *)
      (* List.iter t.destinations ~f:(fun d -> Stdio.printf "%s " d); *)
      (* Stdio.printf "\n"; *)
      t, List.map t.destinations ~f:(fun d -> t.label, false, d)
    | FlipFlop state ->
      if pulse
      then (* Stdio.printf "FlipFlop received high, nothing happens\n"; *)
        t, []
      else
        ( (* Stdio.printf "FlipFlop received low, swapping state and sending signal\n"; *)
          (* Stdio.printf *)
          (*   "%s is sending %s to " *)
          (*   t.label *)
          (*   ((not state) |> fun s -> if s then "high" else "low"); *)
          (* List.iter t.destinations ~f:(fun d -> Stdio.printf "%s " d); *)
          (* Stdio.printf "\n"; *)
          { t with operation = FlipFlop (not state) }
        , List.map t.destinations ~f:(fun d -> t.label, not state, d) )
    | Conjunction table ->
      (* Stdio.printf "\nConjunction operator\n"; *)
      (match Hashtbl.find table sender with
       | None ->
         (* Stdio.printf *)
         (*   "Received %s from new sender: %s\n" *)
         (*   (if pulse then "high" else "low") *)
         (*   sender; *)
         Hashtbl.add_exn table ~key:sender ~data:pulse
       | Some _ ->
         (* Stdio.printf *)
         (*   "Updating signal to %s from sender: %s\n" *)
         (*   (if pulse then "high" else "low") *)
         (*   sender; *)
         Hashtbl.set table ~key:sender ~data:pulse);
      let tbl = Hashtbl.data table |> List.for_all ~f:(fun b -> b) in
      (* if tbl *)
      (* then ( *)
      (*   Stdio.printf "All signals are high\n"; *)
      (*   Stdio.printf *)
      (*     "%s is sending %s to " *)
      (*     t.label *)
      (*     ((not tbl) |> fun s -> if s then "high" else "low"); *)
      (*   List.iter t.destinations ~f:(fun d -> Stdio.printf "%s " d); *)
      (*   Stdio.printf "\n\n") *)
      (* else ( *)
      (*   Stdio.printf "Some signals are low\n"; *)
      (*   Stdio.printf *)
      (*     "%s is sending %s to " *)
      (*     t.label *)
      (*     ((not tbl) |> fun s -> if s then "high" else "low"); *)
      (*   List.iter t.destinations ~f:(fun d -> Stdio.printf "%s " d); *)
      (*   Stdio.printf "\n\n"); *)
      ( { t with operation = Conjunction table }
      , List.map t.destinations ~f:(fun d -> t.label, not tbl, d) )
  ;;
end

module HM = struct
  type t = (string, Mod.t) Hashtbl.t

  let prep_con_mods (t : t) : unit =
    Hashtbl.iter t ~f:(fun m ->
      match m.operation with
      | FlipFlop _ | Broadcast -> ()
      | Conjunction tbl ->
        let mods_that_map_to_me =
          Hashtbl.filter t ~f:(fun m' ->
            List.mem m'.destinations m.label ~equal:String.equal)
          |> Hashtbl.data
          |> List.map ~f:(fun (m' : Mod.t) -> m'.label)
        in
        List.iter mods_that_map_to_me ~f:(fun m' ->
          Hashtbl.add_exn tbl ~key:m' ~data:false))
  ;;

  let press_button (t : t) : bool list * bool =
    let rec go signals acc rx : bool list * bool =
      (* Stdio.printf "\nSo far I've gotten\n"; *)
      (* List.iter acc ~f:(fun b -> Stdio.printf "%s\n" (if b then "high" else "low")); *)
      match signals with
      | [] -> acc, rx
      | (sender, signal, destination) :: ss ->
        let rx = if (not signal) && String.equal destination "rx" then true else false in
        (* Stdio.printf "%s -%s-> %s\n" sender (if signal then "high" else "low") destination; *)
        (match Hashtbl.find t destination with
         | None -> (go [@tailcall]) ss (signal :: acc) rx
         | Some modu ->
           let new_state, new_signals = Mod.process_operation sender signal modu in
           Hashtbl.set t ~key:destination ~data:new_state;
           let new_signals = List.append ss new_signals in
           (* Stdio.printf "\nSignals to process:\n"; *)
           (* List.iter new_signals ~f:(fun (sn, si, de) -> *)
           (* Stdio.printf "%s sent %s to %s\n" sn (if si then "high" else "low") de); *)
           (go [@tailcall]) new_signals (signal :: acc) rx)
    in
    (* go [sender, signal, destination] [accumulated_signals] *)
    go [ "button", false, "broadcaster" ] [] false
  ;;

  let rec press_button_n_times n t acc =
    match n with
    | n when n < 1 -> acc
    | _ ->
      (* Stdio.printf "\n--------------------\nButton pressed\n--------------------\n"; *)
      let acc = List.append acc (fst (press_button t)) in
      (* List.iter (List.rev acc) ~f:(fun b -> *)
      (*   Stdio.printf "%s\n" (if b then "high" else "low")); *)
      press_button_n_times (n - 1) t acc
  ;;

  let rec count_presses_till_rx t result acc =
    match result with
    | true -> acc
    | false ->
      let _, result = press_button t in
      count_presses_till_rx t result acc + 1
  ;;
end

module Angstrom = struct
  include Angstrom

  let destinations : string list t =
    let* s = take_till (fun c -> Char.equal c '\n') in
    (* Stdio.printf "destinations: "; *)
    let destinations = String.split s ~on:',' |> List.map ~f:(fun s -> String.strip s) in
    (* List.iter destinations ~f:(fun d -> Stdio.printf "%s " d); *)
    (* Stdio.printf "\n"; *)
    return destinations
  ;;

  let modu : Mod.t t =
    let* op = char '%' <|> char '&' <|> char 'b' in
    let* label = take_till (fun c -> Char.equal c ' ') in
    let label, operation =
      match op with
      | '%' -> label, FlipFlop false
      | '&' -> label, Conjunction (Hashtbl.create (module String))
      | 'b' -> "broadcaster", Broadcast
      | _ -> invalid_arg "parse operation"
    in
    let* _f = take 4 in
    (* Stdio.printf "\nprocessed %s\noperation: %c\t%s\t" label op _f; *)
    let* destinations = destinations in
    return Mod.{ label; operation; destinations }
  ;;

  let modu_map : HM.t t =
    let* modules = sep_by1 (char '\n') modu in
    let* _ = char '\n' in
    return
      (modules
       |> List.map ~f:(fun (m : Mod.t) -> m.label, m)
       |> Hashtbl.of_alist_exn (module String))
  ;;
end

module Part_1 = struct
  let run (input : string) : (string, string) Result.t =
    let@ modu_map = Angstrom.(parse_string ~consume:All modu_map input) in
    HM.prep_con_mods modu_map;
    (* Hashtbl.iter modu_map ~f:(fun m -> Mod.print m); *)
    let results = HM.press_button_n_times 1000 modu_map [] in
    (* List.iter results ~f:(fun r -> Stdio.printf "%b\n" r); *)
    let t_f =
      List.fold results ~init:(0, 0) ~f:(fun (t, f) r -> if r then t + 1, f else t, f + 1)
    in
    Stdio.printf "highs: %n; lows: %n\n" (fst t_f) (snd t_f);
    let answer = t_f |> (fun (a, b) -> a * b) |> Int.to_string in
    Ok answer
  ;;
end

module Part_2 = struct
  let run (input : string) : (string, string) Result.t =
    let@ modu_map = Angstrom.(parse_string ~consume:All modu_map input) in
    HM.prep_con_mods modu_map;
    (* let answer = HM.count_presses_till_rx modu_map false 0 |> Int.to_string in *)
    Ok "this just stack overflows..., not worth continuing"
  ;;
end
