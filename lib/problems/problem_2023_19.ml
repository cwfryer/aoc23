include Let

let year = 2023
let day = 19
let ( >> ) f g x = g (f x)

(* types to represent the workflows *)
type node = string

type decision =
  | Accept
  | Reject

type op =
  | LT
  | GT

(* result can be an accept/reject decision or another workflow (node) *)
type result =
  | Node of node
  | Decision of decision

(* conditional phrase: reg `op` cnst -> res; reg = "register", i.e., x, m, a, or s *)
type cond =
  { op : op
  ; reg : node
  ; cnst : int
  ; res : result
  }

(* a workflow has a final "else" branch as well as a series of conditional phrases *)
type workflow =
  { final : result
  ; conds : cond list
  }

(* glorious functional parsing *)
let op_of_char = function
  | '<' -> LT
  | '>' -> GT
  | _ -> invalid_arg "op_of_char"
;;

type binding =
  { x : int
  ; m : int
  ; a : int
  ; s : int
  }

module Angstrom = struct
  include Import.Angstrom

  let condition : cond t =
    let* reg = take 1 in
    let* op = char '>' <|> char '<' in
    let op =
      match op with
      | '<' -> LT
      | '>' -> GT
      | _ -> failwith "Invalid sign in input"
    in
    let* cnst = unsigned_int in
    let* _ = char ':' in
    let* res = many1 ascii_letter in
    let res =
      let res = Base.String.of_list res in
      match res with
      | "A" -> Decision Accept
      | "R" -> Decision Reject
      | x -> Node x
    in
    let* _ = char ',' in
    let cond = { op; reg; cnst; res } in
    (* print_cond cond; *)
    return cond
  ;;

  let workflow =
    let* name = take_till (fun c -> Char.equal '{' c) in
    let* _ = char '{' in
    let* conds = many1 condition in
    let* final = take_till (fun c -> Char.equal '}' c) in
    let final =
      match final with
      | "A" -> Decision Accept
      | "R" -> Decision Reject
      | x -> Node x
    in
    let* _ = char '}' in
    let* _ = char '\n' in
    return (name, { final; conds })
  ;;

  let workflows = many1 workflow

  let measure =
    let* l = any_char in
    let* _ = char '=' in
    let* v = unsigned_int in
    return (l, v)
  ;;

  let part =
    let* _ = char '{' in
    let* measures = sep_by1 (char ',') measure in
    let* _ = char '}' in
    let* _ = char '\n' in
    let part =
      Base.List.fold measures ~init:{ x = 0; m = 0; a = 0; s = 0 } ~f:(fun acc (l, v) ->
        match l with
        | 'x' -> { acc with x = v }
        | 'm' -> { acc with m = v }
        | 'a' -> { acc with a = v }
        | 's' -> { acc with s = v }
        | _ -> failwith "Got invalid label for part")
    in
    return part
  ;;

  let parts = many1 part

  let document =
    let* workflows = workflows in
    let workflows = Base.Map.of_alist_exn (module Base.String) workflows in
    let* _ = char '\n' in
    let* parts = parts in
    return (workflows, parts)
  ;;
end

(* settings for the XMAS system (second half of input) *)
module M = Map.Make (String)

let index_binding reg binding =
  match reg with
  | "x" -> binding.x
  | "m" -> binding.m
  | "a" -> binding.a
  | "s" -> binding.s
  | _ -> invalid_arg ("invalid XMAS code " ^ reg)
;;

let eval_cond bind cond =
  (* evaluates a conditional clause given the XMAS settings in bind *)
  match cond.op with
  | LT -> index_binding cond.reg bind < cond.cnst
  | GT -> index_binding cond.reg bind > cond.cnst
;;

let rec eval_at wfs bind wf_name =
  (* why do all the parsing above? so that this function can be stupidly
     concise *)
  let wf = Base.Map.find_exn wfs wf_name in
  match
    wf.conds
    |> List.to_seq
    (* find the first successful conditional *)
    |> Seq.drop_while (eval_cond bind >> not)
    |> Seq.uncons
    (* if there was no successful condition, return wf.final; else, return
       the branch destination of the conditinoal (cond.res) *)
    |> Option.fold ~none:wf.final ~some:(fun (cond, _) -> cond.res)
  with
  | Decision x -> x
  (* workflow goes to another workflow; recurse *)
  | Node name -> eval_at wfs bind name
;;

let func_of_cond cond x =
  match cond.op with
  | LT -> x < cond.cnst
  | GT -> x > cond.cnst
;;

let count_paths wfs =
  (* decidedly less concise than part 1 *)
  (* builds One Giant Lambda to filter on the register ranges (1 .. 4000) while
     traversing the workflow tree from its root. Each edge from a parent to a
     child represents traversing the conditional. As such, a lambda that filters
     according to the conditional is inductively &&'d onto the filter function for
     the appropriate XMAS register at each step. When a leaf is reached, the filter
     functions are applied over the sequence of ints 1 .. 4000, the final counts of
     which are returned back up the tree. *)
  let rec count_paths' filterfuncs res =
    match res with
    | Decision Reject -> 0
    | Decision Accept ->
      (* we've built the filter function, now apply it *)
      let max_rng = 4000 in
      let inp_rng = Seq.ints 1 |> Seq.take max_rng in
      let counter l =
        inp_rng |> Seq.filter (Base.Map.find_exn filterfuncs l) |> Seq.length
      in
      (* each register is independent, so the number of options stack
         multiplicatively *)
      counter "x" * counter "m" * counter "a" * counter "s"
    | Node name ->
      (* we've been sent to another workflow *)
      let wf = Base.Map.find_exn wfs name in
      let counts, elsefuncs =
        (* for each conditional of the current workflow *)
        wf.conds
        |> List.fold_left
             (fun (count, filters) cond ->
               (* function that screens register values according to the
                  conditional described by cond *)
               let condfunc = func_of_cond cond in
               let cur_filterfuncs =
                 Base.Map.update
                   filters
                   cond.reg
                   (* all preceding conditionals must succeed, as must the current one *)
                   ~f:(fun filterfunc x ->
                     match filterfunc with
                     | Some f -> f x && condfunc x
                     | None -> failwith "unsafe_update")
               in
               let cur_counts = count_paths' cur_filterfuncs cond.res in
               (* now consider the branch where the conditional failed. this branch
                  filters on not (condfunc x) *)
               let next_filterfuncs =
                 Base.Map.update filters cond.reg ~f:(fun filterfunc x ->
                   match filterfunc with
                   | Some f -> f x && not (condfunc x)
                   | None -> failwith "unsafe_update")
               in
               (* counts scale additively here because we are partitioning the count
                  among the edges of our tree *)
               count + cur_counts, next_filterfuncs)
             (0, filterfuncs)
      in
      (* consider the case where all of the conditionals are false *)
      let elsecount = count_paths' elsefuncs wf.final in
      counts + elsecount
  in
  let truefn (_ : int) = true in
  let filters =
    Base.Map.of_alist_exn
      (module Base.String)
      [ "x", truefn; "m", truefn; "a", truefn; "s", truefn ]
  in
  count_paths' filters (Node "in")
;;

module Part_1 = struct
  let run (input : string) : (string, string) Result.t =
    let@ document = Angstrom.(parse_string ~consume:All document input) in
    let wfs, binds = document in
    let answer =
      binds
      |> List.filter (fun bind -> eval_at wfs bind "in" == Accept)
      |> List.fold_left (fun acc bind -> acc + bind.x + bind.m + bind.a + bind.s) 0
      |> string_of_int
    in
    Ok answer
  ;;
end

module Part_2 = struct
  let run (input : string) : (string, string) Result.t =
    let@ document = Angstrom.(parse_string ~consume:All document input) in
    let wfs, _binds = document in
    let answer = count_paths wfs |> string_of_int in
    Ok answer
  ;;
end
