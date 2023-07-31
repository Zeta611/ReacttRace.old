open Base
open Stdio

exception Invalid_hook_call
exception Incompatible_useEffect
exception Too_many_cycles

let max_cycles = 25

let update_nth (l : 'a list ref) (n : int) (value : 'a) : unit =
  let old = !l in
  let front, back = List.split_n old n in
  l := front @ [ value ] @ List.drop back 1

let terminal_width () =
  let ic = Unix.open_process_in "tput cols" in
  let result = In_channel.input_line ic in
  let (_ : Unix.process_status) = Unix.close_process_in ic in

  let open Option.Let_syntax in
  (let%map columns = result in
   Int.of_string columns)
  |> Option.value ~default:80

module React = struct
  type value = Univ.univ
  type state = value
  type props = value
  type state_eq = state -> state -> bool

  type component = { name : string; body : props -> ui_element list }
  and ui_element = { component : component; props : props }

  let make_element (type a) (component : component)
      (module U : Univ.S with type t = a) ~(props : a) : ui_element =
    { component; props = U.create props }

  let make_component (type a) ~(name : string)
      (module U : Univ.S with type t = a) (body : a -> ui_element list) :
      component =
    { name; body = (fun props -> body (U.match_exn props)) }

  type state_entry = {
    current : state;
    update : state option;
    equal : state_eq;
  }

  type state_list = state_entry list
  type dependencies = Dependencies of state list | No_dependencies
  type dependency_list = dependencies list
  type effect = unit -> unit
  type effect_queue = effect list

  type tree = { node : data; child : tree list }

  and data = {
    element : ui_element;
    states : state_list ref;
    dependencies : dependency_list ref;
  }

  type path = int list

  let current_states : state_list ref option ref = ref None
  let current_dependencies : dependency_list ref option ref = ref None
  let queued_effects : effect_queue ref = ref []
  let state_index = ref 0
  let effect_index = ref 0
  let create_state_list () : state_list ref = ref []
  let create_dependency_list () : dependency_list ref = ref []

  let run_effects () =
    List.iter !queued_effects ~f:(fun f -> f ());
    queued_effects := []

  let update_states (states : state_list ref) : unit =
    let open List.Let_syntax in
    states :=
      let%map { current; update; equal } = !states in
      let current = Option.value update ~default:current in
      { current; update = None; equal }

  (** After checking changes, updates are set to current *)
  let states_changed ?(during_render = false) (states : state_list ref) : bool =
    let rec loop = function
      | [] -> false
      | { update = None; _ } :: states -> loop states
      | { current; update = Some update; equal } :: states ->
          if during_render then
            (* setting a new state by itself during render should unconditionally trigger re-render:
             * https://stackoverflow.com/a/74036012/5252984 *)
            true
          else (not (equal current update)) || loop states
    in
    let result = loop !states in
    update_states states;
    result

  let rec diff tree : path list =
    if states_changed tree.node.states then [ [] ]
    else
      let indexed_elements = List.mapi ~f:(fun i e -> (i, e)) tree.child in
      let open List.Let_syntax in
      let%bind i, element = indexed_elements in
      let%bind path = diff element in
      return (i :: path)

  let rec extract_subtree tree path : tree =
    match path with
    | [] -> tree
    | i :: path -> extract_subtree (List.nth_exn tree.child i) path

  let rec plugin tree subtree : path -> tree = function
    | [] -> subtree
    | i :: path ->
        let child =
          List.mapi
            ~f:(fun j t -> if i = j then plugin t subtree path else t)
            tree.child
        in
        { tree with child }

  let rec eval ?(prev_tree : tree option)
      ({ component = { name; body }; props } as element : ui_element) : tree =
    let prev_child, states, dependencies =
      match prev_tree with
      | Some { node = { element; states; dependencies }; child }
        when String.(element.component.name = name) ->
          (Some child, states, dependencies)
      | Some _ | None -> (None, create_state_list (), create_dependency_list ())
    in
    state_index := 0;
    effect_index := 0;
    current_states := Some states;
    current_dependencies := Some dependencies;

    (* This will run all the code in the component and returns the child
     * element. This includes running `useState`s and `useEffect`s. *)
    let rec loop () =
      let child_element = body props in
      if states_changed ~during_render:true states then (
        state_index := 0;
        effect_index := 0;
        (* states should not be reset *)
        current_dependencies := Some dependencies;
        loop ())
      else child_element
    in
    let child_element = loop () in

    let child =
      match prev_child with
      | None -> List.map ~f:eval child_element
      | Some prev_child ->
          List.mapi
            ~f:(fun i e -> eval ?prev_tree:(List.nth prev_child i) e)
            child_element
    in
    { node = { element; states; dependencies }; child }

  let reset () =
    current_states := None;
    current_dependencies := None;
    state_index := 0;
    effect_index := 0

  let render (inital_element : ui_element) : ui_element =
    reset ();

    let cycles = ref 0 in
    let initial_tree = eval inital_element in
    let rec loop tree =
      Int.incr cycles;

      let prompt = Printf.sprintf "─( cycle %d )" !cycles in
      let width = Int.max (terminal_width () - String.length prompt) 0 in
      let line = String.concat ~sep:"" (List.init width ~f:(fun _ -> "─")) in
      print_endline (prompt ^ line);

      run_effects ();
      let paths = diff tree in
      if List.is_empty paths then tree.node.element
      else if !cycles >= max_cycles then raise Too_many_cycles
      else
        let open List.Let_syntax in
        let new_trees =
          let%map path = paths in
          let prev_tree = extract_subtree tree path in
          let new_tree = eval ~prev_tree prev_tree.node.element in
          new_tree
        in
        let new_tree = List.fold2_exn ~f:plugin ~init:tree new_trees paths in
        loop new_tree
    in
    loop initial_tree

  let useState (type a) (module U : Univ.S with type t = a) (init : a) :
      a * (a -> unit) =
    match !current_states with
    | None -> raise Invalid_hook_call
    | Some states ->
        let current =
          match List.nth !states !state_index with
          | Some { current; _ } -> current
          | None -> U.create init
        in
        let index_curr = !state_index in
        let setState new_val =
          update_nth states index_curr
            { current; update = Some (U.create new_val); equal = U.( = ) }
        in
        Int.incr state_index;
        (U.match_exn current, setState)

  let useEffect (f : effect) ?(dependencies : (state * state_eq) list option) ()
      : unit =
    match !current_dependencies with
    | None -> raise Invalid_hook_call
    | Some dependency_list ->
        let old_deps = List.nth !dependency_list !effect_index in
        let has_changed =
          match (old_deps, dependencies) with
          | Some (Dependencies old_deps), Some dependencies'
            when List.length old_deps
                 = List.length dependencies' (* Re-render with useEffect *) ->
              List.existsi dependencies' ~f:(fun i (new_state, compare) ->
                  not (compare new_state (List.nth_exn old_deps i)))
          | Some No_dependencies, None (* Re-render with useEffect0 *)
          | None, None (* Initial render with useEffect0 *)
          | None, Some _ (* Initial render with useEffect *) ->
              true
          | _, _ -> raise Incompatible_useEffect
        in
        if has_changed then queued_effects := f :: !queued_effects;
        let new_dependencies =
          match dependencies with
          | Some dependencies -> Dependencies (List.map dependencies ~f:fst)
          | None -> No_dependencies
        in
        update_nth dependency_list !effect_index new_dependencies;
        Int.incr effect_index
end
