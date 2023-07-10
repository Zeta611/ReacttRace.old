open Base

module React : sig
  val render : (unit -> (< render : unit ; .. > as 'a)) -> 'a
  val useState : 'a -> 'a * ('a -> unit)
  val useEffect : (unit -> unit) -> 'a list -> unit
  val useEffect0 : (unit -> unit) -> unit
end = struct
  let states = Hashtbl.create (module Int)
  let effects = Hashtbl.create (module Int)
  let state_index = ref 0
  let effect_index = ref 0
  let ( !! ) = Stdlib.Obj.magic

  let render component =
    state_index := 0;
    effect_index := 0;
    let c = component () in
    c#render;
    c

  let useState init =
    let state =
      match Hashtbl.find states !state_index with
      | Some state -> !!state
      | None -> init
    in
    let index_curr = !state_index in
    let setState new_val = Hashtbl.set states ~key:index_curr ~data:!!new_val in
    Int.incr state_index;
    (state, setState)

  let useEffect f dependencies =
    let old_deps = !!(Hashtbl.find effects !effect_index) in
    let has_changed =
      match old_deps with
      | Some old_deps ->
          List.existsi dependencies ~f:(fun i d ->
              Poly.(d <> List.nth_exn old_deps i))
      | None -> true
    in
    if has_changed then f ();
    Hashtbl.set effects ~key:!effect_index ~data:!!dependencies;
    Int.incr effect_index

  let useEffect0 f = f ()
end
