module React : sig
  type state = Univ.t
  type props = Univ.t

  type component = Null | Composite of composite_component
  and composite_component = { name : string; body : props -> ui_element list }
  and ui_element = { component : component; props : props }

  val render : ui_element -> ui_element
  val useState : state -> state * (state -> unit)

  val useEffect :
    (unit -> unit) -> (state * (state -> state -> bool)) list -> unit

  val useEffect0 : (unit -> 'a) -> 'a
  val reset : unit -> unit
end
