exception Invalid_hook_call
exception Incompatible_useEffect
exception Too_many_cycles

module React : sig
  type value = Univ.univ
  type state = value
  type props = value
  type state_eq = value -> value -> bool
  type ui_element
  type component

  val make_element :
    component -> (module Univ.S with type t = 'a) -> props:'a -> ui_element

  val make_component :
    name:string ->
    (module Univ.S with type t = 'a) ->
    ('a -> ui_element list) ->
    component

  type effect = unit -> unit

  val render : ui_element -> ui_element
  val useState : (module Univ.S with type t = 'a) -> 'a -> 'a * ('a -> unit)

  val useEffect :
    effect -> ?dependencies:(state * state_eq) list -> unit -> unit
end
