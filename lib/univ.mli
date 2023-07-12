(* Signature taken from https://blog.janestreet.com/rethinking-univ/ *)

module Variant : sig
  type 'a t

  val create : unit -> 'a t
end

type t

val create : 'a Variant.t -> 'a -> t
val match_ : 'a Variant.t -> t -> 'a option
val match_exn : 'a Variant.t -> t -> 'a

module Make_univ (T : sig
  type t

  val ( = ) : t -> t -> bool
end) : sig
  val ( = ) : t -> t -> bool
  val create : T.t -> t
  val match_ : t -> T.t option
  val match_exn : t -> T.t
end
