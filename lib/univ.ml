open Base

(* See also:
   https://web.archive.org/web/20080511203848/http://ocaml.janestcapital.com/?q=node/18
   http://mlton.org/UniversalType
   https://web.archive.org/web/20110818210314/http://eigenclass.org/R2/writings/heterogeneous-containers-in-ocaml
   https://web.archive.org/web/20080315021028/https://caml.inria.fr/pub/ml-archives/caml-list/2001/05/670222a47a96487f236602e00781f497.en.html
*)

module Variant = struct
  type 'a t = ('a -> exn) * (exn -> 'a option)

  let create (type s) () =
    let exception E of s in
    ((fun x -> E x), function E x -> Some x | _ -> None)
end

type t = exn

let create (type a) (variant : a Variant.t) (value : a) : t =
  (fst variant) value

let match_ (type a) (variant : a Variant.t) (value : t) : a option =
  (snd variant) value

let match_exn (type a) (variant : a Variant.t) (value : t) : a =
  match (snd variant) value with
  | Some x -> x
  | None -> raise (Invalid_argument "match_exn")

module Make_univ (T : sig
  type t

  val ( = ) : t -> t -> bool
end) : sig
  val ( = ) : t -> t -> bool
  val create : T.t -> t
  val match_ : t -> T.t option
  val match_exn : t -> T.t
end = struct
  let typ = Variant.create ()

  let ( = ) a b =
    match (match_ typ a, match_ typ b) with
    | Some a, Some b -> T.(a = b)
    | _ -> raise (Invalid_argument "int_compare")

  let create x = create typ x
  let match_ x = match_ typ x
  let match_exn x = match_exn typ x
end
