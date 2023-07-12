open Base

module React = struct
  type state = Univ.t

  let states : (int, state) Hashtbl.t = Hashtbl.create (module Int)
  let effects : (int, state list) Hashtbl.t = Hashtbl.create (module Int)
  let state_index = ref 0
  let effect_index = ref 0

  let render component =
    state_index := 0;
    effect_index := 0;
    let c = component () in
    c#render;
    c

  let useState (init : state) : state * (state -> unit) =
    let state =
      match Hashtbl.find states !state_index with
      | Some state -> state
      | None -> init
    in
    let index_curr = !state_index in
    let setState new_val = Hashtbl.set states ~key:index_curr ~data:new_val in
    Int.incr state_index;
    (state, setState)

  let useEffect (f : unit -> unit)
      (dependencies : (state * (state -> state -> bool)) list) : unit =
    let old_deps = Hashtbl.find effects !effect_index in
    let has_changed =
      match old_deps with
      | Some old_deps ->
          List.existsi dependencies ~f:(fun i (new_state, compare) ->
              not (compare new_state (List.nth_exn old_deps i)))
      | None -> true
    in
    if has_changed then f ();
    Hashtbl.set effects ~key:!effect_index ~data:(List.map dependencies ~f:fst);
    Int.incr effect_index

  let useEffect0 f = f ()

  let reset () =
    Hashtbl.clear states;
    Hashtbl.clear effects;
    state_index := 0;
    effect_index := 0
end
