include Core
include Let
include Fn

let ( << ) f g x = f (g x)

module Int = struct
  include Int

  let sum (xs : int list) : int = List.fold_left xs ~init:0 ~f:( + )
  let product (xs : int list) : int = List.fold_left xs ~init:1 ~f:( * )

  let maximum (xs : int list) : int =
    List.fold_left xs ~f:max ~init:Int.min_value

  let minimum (xs : int list) : int =
    List.fold_left xs ~f:min ~init:Int.max_value
end

module Table = struct
  module Map = Stdlib.Map

  module type KEY = sig
    include Map.OrderedType
  end

  module type T = sig
    module K1 : KEY
    module K2 : KEY

    type 'v t

    (* Constructors *)

    val empty : 'v t
    val group_by : ('v -> K1.t) -> ('v -> K2.t) -> 'v list -> 'v t
    val of_entries : (K1.t * K2.t * 'v) list -> 'v t

    (* Mutators *)

    val map : ('a -> 'b) -> 'a t -> 'b t
    val add : K1.t * K2.t -> 'v -> 'v t -> 'v t
    val remove : K1.t * K2.t -> 'v t -> 'v t
    val update : K1.t * K2.t -> ('v option -> 'v option) -> 'v t -> 'v t

    (* Accessors *)
    val keys : 'a t -> K2.t list
    val keys2 : 'a t -> (K1.t * K2.t) list
    val size : 'a t -> int
    val contains : K1.t * K2.t -> 'v t -> bool
    val find_opt : K1.t -> K2.t -> 'v t -> 'v option
    val find_all : f:('v -> bool) -> 'v t -> (K1.t * K2.t * 'v) list
  end

  module Make (K1 : KEY) (K2 : KEY) :
    T with type K1.t = K1.t and type K2.t = K2.t = struct
    module K1 = K1
    module K2 = K2
    module Map1 = Map.Make (K1)
    module Map2 = Map.Make (K2)

    type 'v t = 'v Map1.t Map2.t

    let empty = Map2.empty
    let map (f : 'a -> 'b) : 'a t -> 'b t = Map2.map (Map1.map f)
    let keys (t : 'a t) : K2.t list = List.map ~f:fst @@ Map2.bindings t

    let keys2 (t : 'a t) : (K1.t * K2.t) list =
      let keys2 = keys t in
      List.concat_map
        ~f:(fun k2 ->
          let m1 = Map2.find k2 t in
          let k1s = List.map ~f:fst @@ Map1.bindings m1 in
          List.map ~f:(fun k1 -> (k1, k2)) k1s)
        keys2

    let size (m : 'v t) : int = List.length @@ keys2 m

    let contains ((k1, k2) : K1.t * K2.t) (m2 : 'v t) : bool =
      match Map2.find_opt k2 m2 with None -> false | Some m1 -> Map1.mem k1 m1

    let find_opt : K1.t -> K2.t -> 'v t -> 'v option =
     fun k1 k2 m2 ->
      let- m1 = Map2.find_opt k2 m2 in
      Map1.find_opt k1 m1

    let find_all ~(f : 'v -> bool) (m2 : 'v t) : (K1.t * K2.t * 'v) list =
      let ( let* ) x f = List.concat_map ~f x in
      let* k2, m1 = Map2.bindings m2 in
      let* k1, v = Map1.bindings m1 in
      match f v with true -> [ (k1, k2, v) ] | false -> []

    let add ((k1, k2) : K1.t * K2.t) (v : 'v) (m : 'v t) : 'v t =
      Map2.update k2
        (function
          | None -> Some (Map1.singleton k1 v)
          | Some m1 -> Some (Map1.add k1 v m1))
        m

    let remove ((k1, k2) : K1.t * K2.t) (m : 'v t) : 'v t =
      Map2.update k2
        (function None -> None | Some m1 -> Some (Map1.remove k1 m1))
        m

    let update ((k1, k2) : K1.t * K2.t) (f : 'v option -> 'v option) (m : 'v t)
        : 'v t =
      match f @@ find_opt k1 k2 m with
      | None -> remove (k1, k2) m
      | Some v -> add (k1, k2) v m

    let group_by (k1_fn : 'v -> K1.t) (k2_fn : 'v -> K2.t) (vs : 'v list) : 'v t
        =
      List.fold_left
        ~f:(fun m2 next ->
          let k1, k2 = (k1_fn next, k2_fn next) in
          Map2.update k2
            (function
              | None -> Some (Map1.singleton k1 next)
              | Some m1 -> Some (Map1.add k1 next m1))
            m2)
        ~init:empty vs

    let of_entries (es : (K1.t * K2.t * 'v) list) : 'v t =
      List.fold_left
        ~f:(fun m2 (k1, k2, v) ->
          Map2.update k2
            (function
              | None -> Some (Map1.singleton k1 v)
              | Some m1 -> Some (Map1.add k1 v m1))
            m2)
        ~init:empty es
  end
end

module Angstrom = struct
  include Angstrom

  let unsigned_int : int t =
    int_of_string <$> take_while1 @@ function '0' .. '9' -> true | _ -> false

  let negative_int : int t =
    let* _ = char '-' in
    let* n = unsigned_int in
    return (-n)

  let signed_int : int t = unsigned_int <|> negative_int

  let lowercase_ascii : char t =
    choice
    @@ List.init 26 ~f:(fun x -> char @@ char_of_int (x + int_of_char 'a'))

  let uppercase_ascii : char t =
    choice
    @@ List.init 26 ~f:(fun x -> char @@ char_of_int (x + int_of_char 'A'))

  let ascii_letter : char t = uppercase_ascii <|> lowercase_ascii

  let lowercase_hex : char t =
    choice (List.init 6 ~f:(fun x -> char @@ char_of_int (x + int_of_char 'a')))
    <|> choice
          (List.init 10 ~f:(fun x -> char @@ char_of_int (x + int_of_char '0')))

  let hex_nibble : int t =
    let* c = lowercase_hex in
    match c with
    | '0' .. '9' as c -> return @@ (int_of_char c - int_of_char '0')
    | 'a' .. 'f' as c -> return @@ (int_of_char c - int_of_char 'a' + 10)
    | c -> fail @@ Fmt.str "Invalid hex nibble: '%c'" c

  let hex_byte : int t =
    let* high = hex_nibble in
    let* low = hex_nibble in
    return @@ ((high lsl 4) lor low)
end

module Grid = struct
  (**************************************************************************)
  (*                                                                        *)
  (*  Copyright (C) Jean-Christophe Filliatre                               *)
  (*                                                                        *)
  (*  This software is free software; you can redistribute it and/or        *)
  (*  modify it under the terms of the GNU Lesser General Public            *)
  (*  License version 2.1, with the special exception on linking            *)
  (*  described in file LICENSE.                                            *)
  (*                                                                        *)
  (*  This software is distributed in the hope that it will be useful,      *)
  (*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
  (*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
  (*                                                                        *)
  (**************************************************************************)

  type 'a t = 'a array array
  type position = int * int

  let height g = Array.length g
  let width g = Array.length g.(0)
  let size g = (height g, width g)

  let make h w v =
    if h < 1 || w < 1 then invalid_arg "Grid.make";
    Array.make_matrix ~dimx:h ~dimy:w v

  let init h w f =
    if h < 1 || w < 1 then invalid_arg "Grid.init";
    Array.init h ~f:(fun i -> Array.init w ~f:(fun j -> f (i, j)))

  let inside g (i, j) = 0 <= i && i < height g && 0 <= j && j < width g
  let get g (i, j) = g.(i).(j)
  let set g (i, j) v = g.(i).(j) <- v

  type direction = N | NW | W | SW | S | SE | E | NE

  let move d (i, j) =
    match d with
    | N -> (i - 1, j)
    | NW -> (i - 1, j - 1)
    | W -> (i, j - 1)
    | SW -> (i + 1, j - 1)
    | S -> (i + 1, j)
    | SE -> (i + 1, j + 1)
    | E -> (i, j + 1)
    | NE -> (i - 1, j + 1)

  let north = move N
  let north_west = move NW
  let west = move W
  let south_west = move SW
  let south = move S
  let south_east = move SE
  let east = move E
  let north_east = move NE

  let rotate_left g =
    let h = height g and w = width g in
    init w h (fun (i, j) -> g.(j).(w - 1 - i))

  let rotate_right g =
    let h = height g and w = width g in
    init w h (fun (i, j) -> g.(h - 1 - j).(i))

  let map f g = init (height g) (width g) (fun p -> f p (get g p))

  let iter4 f g p =
    let f p = if inside g p then f p (get g p) in
    f (north p);
    f (west p);
    f (south p);
    f (east p)

  let iter8 f g (i, j) =
    for di = -1 to 1 do
      for dj = -1 to 1 do
        if di <> 0 || dj <> 0 then
          let p = (i + di, j + dj) in
          if inside g p then f p (get g p)
      done
    done

  let fold4 f g p acc =
    let f p acc = if inside g p then f p (get g p) acc else acc in
    acc |> f (north p) |> f (west p) |> f (south p) |> f (east p)

  let fold8 f g p acc =
    let f p acc = if inside g p then f p (get g p) acc else acc in
    acc
    |> f (north p)
    |> f (north_west p)
    |> f (west p)
    |> f (south_west p)
    |> f (south p)
    |> f (south_east p)
    |> f (east p)
    |> f (north_east p)

  let iter f g =
    for i = 0 to height g - 1 do
      for j = 0 to width g - 1 do
        f (i, j) g.(i).(j)
      done
    done

  let fold f g acc =
    let rec fold ((i, j) as p) acc =
      if i = height g then acc
      else if j = width g then fold (i + 1, 0) acc
      else fold (i, j + 1) (f p g.(i).(j) acc)
    in
    fold (0, 0) acc

  let find f g =
    let exception Found of position in
    let exception Not_found in
    try
      iter (fun p c -> if f p c then raise (Found p)) g;
      raise Not_found
    with Found p -> p

  let read c =
    let rec scan rows =
      match In_channel.input_line_exn c with
      | s -> scan (s :: rows)
      | exception End_of_file ->
          let row s = Array.init (String.length s) ~f:(String.get s) in
          let g =
            Array.map (Array.of_list (List.rev rows)) ~f:(fun r -> row r)
          in
          if Array.length g = 0 then invalid_arg "Grid.read";
          let w = Array.length g.(0) in
          for i = 1 to height g - 1 do
            if Array.length g.(i) <> w then invalid_arg "Grid.read"
          done;
          g
    in
    scan []

  let print ?(bol = fun _fmt _i -> ())
      ?(eol = fun fmt _i -> Format.pp_print_newline fmt ())
      ?(sep = fun _fmt _p -> ()) p fmt g =
    for i = 0 to height g - 1 do
      bol fmt i;
      for j = 0 to width g - 1 do
        p fmt (i, j) g.(i).(j);
        if j < width g - 1 then sep fmt (i, j)
      done;
      eol fmt i
    done

  let print_chars = print (fun fmt _ c -> Format.pp_print_char fmt c)
end
