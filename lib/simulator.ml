open Base
(* open Stdio *)

module React = struct
  type state = Univ.t
  type props = Univ.t

  type component = Null | Composite of composite_component
  and composite_component = { name : string; body : props -> ui_element list }
  and ui_element = { component : component; props : props }

  type view_tree =
    | Leaf_node
    | Tree of {
        name : string;
        children : view_tree list;
        states : (int, state) Hashtbl.t;
        effects : (int, state list) Hashtbl.t;
      }

  type state_list = (int, state) Hashtbl.t
  type effect_list = (int, state list) Hashtbl.t

  let view_tree : view_tree option ref = ref None
  let current_states : state_list option ref = ref None
  let current_effects : effect_list option ref = ref None
  let state_index = ref 0
  let effect_index = ref 0
  let create_state_list () = Hashtbl.create (module Int)
  let create_effect_list () = Hashtbl.create (module Int)

  let set_states_effects states effects =
    state_index := 0;
    effect_index := 0;
    current_states := Some states;
    current_effects := Some effects

  let render (element : ui_element) : ui_element =
    let rec children_tree ?prev_bundle { name; body } props =
      let prev_children, states, effects =
        match prev_bundle with
        | None -> (None, create_state_list (), create_effect_list ())
        | Some (prev_children, prev_states, prev_effects) ->
            (Some prev_children, prev_states, prev_effects)
      in

      set_states_effects states effects;

      let children = body props in
      let child_trees =
        match prev_children with
        | None ->
            List.map
              ~f:(fun child -> render_with_tree child None |> snd)
              children
        | Some prev_children ->
            List.mapi
              ~f:(fun i child ->
                render_with_tree child (List.nth prev_children i) |> snd)
              children
      in
      Tree { name; children = child_trees; states; effects }
    and render_with_tree ({ component; props } as ui_element) view_tree =
      let new_tree =
        match component with
        | Null -> Leaf_node
        | Composite ({ name; _ } as c) -> (
            match view_tree with
            | None (* Initial render *) | Some Leaf_node (* Re-render *) ->
                (* There is no tree to follow *)
                children_tree c props
            | Some
                (Tree
                  {
                    name = prev_name;
                    children = prev_children;
                    states = prev_states;
                    effects = prev_effects;
                  }) ->
                (* Re-render *)
                let replaced = String.(name <> prev_name) in
                if replaced then
                  (* No longer need--*cannot*--to track the previous tree *)
                  children_tree c props
                else
                  (* Keep track of the previous tree structure to mirror the recursion *)
                  children_tree
                    ~prev_bundle:(prev_children, prev_states, prev_effects)
                    c props)
      in
      (ui_element, new_tree)
    in
    let element, new_tree = render_with_tree element !view_tree in
    view_tree := Some new_tree;
    element

  let useState (init : state) : state * (state -> unit) =
    match !current_states with
    | None -> failwith "useState called before render"
    | Some states ->
        let state =
          match Hashtbl.find states !state_index with
          | Some state -> state
          | None -> init
        in
        let index_curr = !state_index in
        let setState new_val =
          Hashtbl.set states ~key:index_curr ~data:new_val
        in
        Int.incr state_index;
        (state, setState)

  let useEffect (f : unit -> unit)
      (dependencies : (state * (state -> state -> bool)) list) : unit =
    match !current_effects with
    | None -> failwith "useEffect called before render"
    | Some effects ->
        let old_deps = Hashtbl.find effects !effect_index in
        let has_changed =
          match old_deps with
          | Some old_deps ->
              List.existsi dependencies ~f:(fun i (new_state, compare) ->
                  not (compare new_state (List.nth_exn old_deps i)))
          | None -> true
        in
        if has_changed then f ();
        Hashtbl.set effects ~key:!effect_index
          ~data:(List.map dependencies ~f:fst);
        Int.incr effect_index

  let useEffect0 f = f ()

  let reset () =
    view_tree := None;
    current_states := None;
    current_effects := None;
    state_index := 0;
    effect_index := 0
end
