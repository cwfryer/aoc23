include Core
include Let
include Fn
include Str

let ( << ) f g x = f (g x)

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
