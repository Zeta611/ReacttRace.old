open Base

(* See also:
   https://web.archive.org/web/20080511203848/http://ocaml.janestcapital.com/?q=node/18
   http://mlton.org/UniversalType
   https://web.archive.org/web/20110818210314/http://eigenclass.org/R2/writings/heterogeneous-containers-in-ocaml
   https://web.archive.org/web/20080315021028/https://caml.inria.fr/pub/ml-archives/caml-list/2001/05/670222a47a96487f236602e00781f497.en.html
*)

type univ = exn

module Variant = struct
  type 'a t = ('a -> univ) * (univ -> 'a option)

  let create (type a) () =
    let exception E of a in
    ((fun x -> E x), function E x -> Some x | _ -> None)
end

module type S = sig
  type t

  val ( = ) : univ -> univ -> bool
  val create : t -> univ
  val match_ : univ -> t option
  val match_exn : univ -> t
end

module Make_univ (T : sig
  type t

  val ( = ) : t -> t -> bool
end) : S with type t = T.t = struct
  type t = T.t

  let typ = Variant.create ()
  let create x = (fst typ) x
  let match_ x = (snd typ) x

  let ( = ) a b =
    match (match_ a, match_ b) with
    | Some a, Some b -> T.(a = b)
    | _ -> raise (Invalid_argument "int_compare")

  let match_exn x =
    match match_ x with
    | Some x -> x
    | None -> raise (Invalid_argument "match_exn")
end
