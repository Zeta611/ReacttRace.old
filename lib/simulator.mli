module React : sig
  type state = Univ.t

  val render : (unit -> (< render : unit ; .. > as 'a)) -> 'a
  val useState : state -> state * (state -> unit)

  val useEffect :
    (unit -> unit) -> (state * (state -> state -> bool)) list -> unit

  val useEffect0 : (unit -> unit) -> unit
  val reset : unit -> unit
end
