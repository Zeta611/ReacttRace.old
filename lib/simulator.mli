exception Invalid_hook_call
exception Incompatible_useEffect

module React : sig
  type state = Univ.t
  type props = Univ.t
  type state_eq = state -> state -> bool
  type effect = unit -> unit

  type component = Null | Composite of composite_component
  and composite_component = { name : string; body : props -> ui_element list }
  and ui_element = { component : component; props : props }

  val render : ui_element -> ui_element
  val useState : state -> state * (state -> unit)

  val useEffect :
    effect -> ?dependencies:(state * state_eq) list -> unit -> unit

  val reset : unit -> unit
end
