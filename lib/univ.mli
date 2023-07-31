(* Signature taken from https://blog.janestreet.com/rethinking-univ/ *)

type univ

module Variant : sig
  type 'a t = ('a -> univ) * (univ -> 'a option)

  val create : unit -> ('a -> univ) * (univ -> 'a option)
end

module type S = sig
  type t

  val ( = ) : univ -> univ -> bool
  val create : t -> univ
  val match_ : univ -> t option
  val match_exn : univ -> t
end

module Make_univ : functor
  (T : sig
     type t

     val ( = ) : t -> t -> bool
   end)
  -> S with type t = T.t
