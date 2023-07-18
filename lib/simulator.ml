open Base
open Stdio

module React = struct
  type state = Univ.t
  type props = Univ.t

  type component =
    | Null
    | Composite of { name : string; body : props -> ui_element }

  and ui_element = { component : component; props : props }

  type virtual_dom =
    | Leaf_node
    | Tree of {
        name : string;
        children : virtual_dom list;
        states : (int, state) Hashtbl.t;
        effects : (int, state list) Hashtbl.t;
      }

  let virtual_dom : virtual_dom option ref = ref None
  let current_states : (int, state) Hashtbl.t option ref = ref None
  let current_effects : (int, state list) Hashtbl.t option ref = ref None
  let state_index = ref 0
  let effect_index = ref 0

  let render element =
    let rec inner { component; props } =
      let states = Hashtbl.create (module Int) in
      let effects = Hashtbl.create (module Int) in
      current_states := Some states;
      current_effects := Some effects;
      state_index := 0;
      effect_index := 0;

      let current_tree =
        match component with
        | Null -> Leaf_node
        | Composite { name; body = f } ->
            printf "Rendering %s\n" name;
            let child = f props in
            Tree
              {
                name;
                children = [ inner child |> snd ];
                states;
                effects;
              }
      in
      ({ component; props }, current_tree)
    in
    (* Re-render not implemented *)
    assert (Option.is_none !virtual_dom);
    let element, tree = inner element in
    virtual_dom := Some tree;
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
    virtual_dom := None;
    current_states := None;
    current_effects := None;
    state_index := 0;
    effect_index := 0
end

(* module React = struct
     type state = Univ.t

     type _ component =
       | Null : unit component
       | Composite : string * ('props -> ui_element_wrapper) -> 'props component

     and 'props ui_element = { component : 'props component; props : 'props }
     and  ui_element_wrapper = < element : 'props. 'props ui_element >

     type virtual_dom =
       | Leaf_node
       | Tree of {
           name : string;
           children : virtual_dom list;
           states : (int, state) Hashtbl.t;
           effects : (int, state list) Hashtbl.t;
         }

     let virtual_dom : virtual_dom option ref = ref None
     let current_states : (int, state) Hashtbl.t option ref = ref None
     let current_effects : (int, state list) Hashtbl.t option ref = ref None
     let state_index = ref 0
     let effect_index = ref 0

     let render element =
       let rec inner :
           type props. props ui_element -> props ui_element * virtual_dom =
        fun element ->
         let { component; props } = element in

         let states = Hashtbl.create (module Int) in
         let effects = Hashtbl.create (module Int) in
         current_states := Some states;
         current_effects := Some effects;
         state_index := 0;
         effect_index := 0;

         let current_tree =
           match component with
           | Null -> Leaf_node
           | Composite (name, f) ->
               let child = f props in
               Tree { name; children = [ inner child |> snd ]; states; effects }
         in
         ({ component; props }, current_tree)
       in
       (* Re-render not implemented *)
       assert (Option.is_none !virtual_dom);
       let element, tree = inner element in
       virtual_dom := Some tree;
       element

     (* let render component =
       state_index := 0;
       effect_index := 0;
       let c = component () in
       c#render;
       c *)

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
       virtual_dom := None;
       current_states := None;
       current_effects := None;
       state_index := 0;
       effect_index := 0
   end *)
