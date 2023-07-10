open Base
open Monads.Std

type expression =
  | Constant of int
  | Variable of string
  (* | Empty_view
     | View_pair of { left : expression; right : expression }
     | Sequence of { first : expression; second : expression } *)
  | Operator of { operator : string; lhs : expression; rhs : expression }
  | If0 of {
      predicate : expression;
      consequent : expression;
      alternative : expression;
    }
  | Application of { func : expression; argument : expression }
  | Rec_bind of { name : string; body : expression }
  | Function of { argument : string; body : expression }
  | Component of { argument : string; body : expression }
    (* | state_bind of { name : string; initial_state : expression; body : expression }
       | state_update of { name : string; new_state : expression }
       (* first only consider effects that run every (re-)renders *)
       | Effect of { (* dependency : string list ; *) effect : expression } *)
[@@deriving show { with_path = false }]

type location = int

type value =
  | Unit
  | Number of int
  | Closure of {
      argument : string;
      body : expression;
      environment : environment;
    }
  | Component_closure of {
      argument : string;
      body : expression;
      environment : environment;
    }
  | Ui_element of {
      value : value;
      body : expression;
      environment : environment;
      states : state list;
      effects : effect list;
    }

and environment = (string, location, String.comparator_witness) Map.t
and store = (location, value, Int.comparator_witness) Map.t
and state = location
and effect = expression * environment

exception Unbound_operator of string
exception Type_error of { op : string; value : value }

let show_environment (env : environment) : string =
  let show_pair (x, loc) = Printf.sprintf "%s : %d" x loc in
  let pairs = Map.to_alist env in
  let pairs_str = List.map pairs ~f:show_pair |> String.concat ~sep:", " in
  if String.is_empty pairs_str then "{}" else Printf.sprintf "{ %s }" pairs_str

let show_value : value -> string = function
  | Unit -> "()"
  | Number n -> "Number" ^ Int.to_string n
  | Closure { argument; body; environment = env } ->
      Printf.sprintf "<(fun %s -> %s), %s>" argument (show_expression body)
        (show_environment env)
  | Component_closure { argument; body; environment = env } ->
      Printf.sprintf "<(com %s -> %s), %s>" argument (show_expression body)
        (show_environment env)
  | Ui_element _ -> "<UI>"

let show_store (store : store) : string =
  let show_pair (loc, v) = Printf.sprintf "%d : %s" loc (show_value v) in
  let pairs = Map.to_alist store in
  let concatenated = List.map pairs ~f:show_pair |> String.concat ~sep:", " in
  if String.is_empty concatenated then "{}"
  else Printf.sprintf "{ %s }" concatenated

let rec fix f x = f (fix f) x

module type Computation = sig
  type 'a t

  include Monad.Core with type 'a t := 'a t
  include Monad.Trans.S with type 'a t := 'a t
  include Monad.Syntax.Let.S with type 'a t := 'a t
end

module type Components = sig
  type 'a t
  type result
  type evaluation = expression -> value t

  module M : Computation with type 'a t := 'a t

  val run_operator : string -> value -> value -> value t
  val fail : exn -> value t
  val find : location -> value t
  val allocate : string -> location t
  val extend : location -> value -> unit t
  val ask_environment : unit -> environment t
  val find_environment : environment -> string -> location
  val augment_environment : environment -> string -> location -> environment
  val local_environment : environment -> value t -> value t
  val empty_effects : unit -> unit t
  val get_effects : unit -> effect list t
  val empty_states : unit -> unit t
  val get_states : unit -> state list t
  val evaluate : (evaluation -> evaluation) -> expression -> result
end

module type Interpreter = sig
  type result

  val evaluate : expression -> result
end

module Make_interpreter (C : Components) :
  Interpreter with type result := C.result = struct
  type result = C.result
  type evaluation = C.evaluation

  let eval (eval : evaluation) : evaluation =
    let open C in
    let open M in
    function
    | Constant n -> return (Number n)
    | Variable x ->
        let* env = ask_environment () in
        find (find_environment env x)
    | If0 { predicate; consequent; alternative } -> (
        let* v = eval predicate in
        match v with
        | Number 0 -> eval consequent
        | Number _ -> eval alternative
        | _ -> fail (Type_error { op = "if0"; value = v }))
    | Operator { operator; lhs; rhs } ->
        let* lhs = eval lhs in
        let* rhs = eval rhs in
        run_operator operator lhs rhs
    | Rec_bind { name; body } ->
        let* env = ask_environment () in
        let* loc = allocate name in
        let env' = augment_environment env name loc in
        let* v = local_environment env' (eval body) in
        let* () = extend loc v in
        return v
    | Function { argument; body } ->
        let* environment = ask_environment () in
        return (Closure { argument; body; environment })
    | Component { argument; body } ->
        let* environment = ask_environment () in
        return (Component_closure { argument; body; environment })
    | Application { func; argument } -> (
        let* closure = eval func in
        match closure with
        | Closure { argument = x; body; environment } ->
            let* v = eval argument in
            let* loc = allocate x in
            let* () = extend loc v in
            let environment' = augment_environment environment x loc in
            local_environment environment' (eval body)
        | Component_closure { argument = x; body; environment } ->
            let* v = eval argument in
            let* loc = allocate x in
            let* () = extend loc v in
            let env' = augment_environment environment x loc in
            let* () = empty_states () in
            let* () = empty_effects () in
            let* value = local_environment env' (eval body) in
            let* effects = get_effects () in
            let* states = get_states () in
            return (Ui_element { value; body; environment; states; effects })
        | _ -> fail (Type_error { op = "application"; value = closure }))

  let evaluate (e : expression) : result = C.evaluate eval e
end

type default_result = {
  result : ((value * effect list) * state list, exn) Result.t;
  store : store;
}

module Default_components : Components with type result = default_result =
struct
  type result = default_result

  module Store = struct
    module T = struct
      type t = store
    end

    include Monad.State.T1 (T) (Monad.Ident)
    include Monad.State.Make (T) (Monad.Ident)

    let empty = Map.empty (module Int)
  end

  module ExnState = struct
    include Monad.Result.Exception.T (Store)
    include Monad.Result.Exception.Make (Store)
  end

  module States = struct
    module T = struct
      type t = state list
      (* include Monoid.List.Make (struct
           type t = state
         end) *)
    end

    include Monad.State.T1 (T) (ExnState)
    include Monad.State.Make (T) (ExnState)

    let empty : state list = []
  end

  module Effects = struct
    module T = struct
      type t = effect list
      (* include Monoid.List.Make (struct
           type t = effect
         end) *)
    end

    include Monad.State.T1 (T) (States)
    include Monad.State.Make (T) (States)

    let empty : effect list = []
  end

  module Environment = struct
    module T = struct
      type t = environment
    end

    include Monad.Reader.T1 (T) (Effects)
    include Monad.Reader.Make (T) (Effects)

    let empty = Map.empty (module String)
  end

  module M = Environment

  type 'a t = 'a M.t
  type evaluation = expression -> value t

  let run_operator (op : string) (lhs : value) (rhs : value) : value M.t =
    let m =
      let open ExnState in
      match (op, lhs, rhs) with
      | "+", Number lhs, Number rhs -> return (Number (lhs + rhs))
      | "-", Number lhs, Number rhs -> return (Number (lhs - rhs))
      | "*", Number lhs, Number rhs -> return (Number (lhs * rhs))
      | "/", Number lhs, Number rhs ->
          if rhs = 0 then fail Division_by_zero else return (Number (lhs / rhs))
      | op, Number _, Number _ -> fail (Unbound_operator op)
      | op, Number _, _ -> fail (Type_error { op; value = rhs })
      | op, _, _ -> fail (Type_error { op; value = lhs })
    in
    M.lift @@ Effects.lift @@ States.lift @@ m

  let fail (e : exn) : value M.t =
    M.lift @@ Effects.lift @@ States.lift @@ ExnState.fail e

  let find (loc : location) : value M.t =
    let m =
      let open Store in
      let* store = get () in
      let value = Map.find_exn store loc in
      return value
    in
    M.lift @@ Effects.lift @@ States.lift @@ ExnState.lift @@ m

  let allocate (_x : string) : location M.t =
    let m =
      let open Store in
      let* store = get () in
      let new_loc = Map.length store in
      return new_loc
    in
    M.lift @@ Effects.lift @@ States.lift @@ ExnState.lift @@ m

  let extend (loc : location) (v : value) : unit M.t =
    let m =
      let open Store in
      let* store = get () in
      let store' = Map.set store ~key:loc ~data:v in
      put store'
    in
    M.lift @@ Effects.lift @@ States.lift @@ ExnState.lift @@ m

  let ask_environment : unit -> environment M.t = M.read

  let find_environment (env : environment) (x : string) : location =
    Map.find_exn env x

  let augment_environment (env : environment) (x : string) (loc : location) :
      environment =
    Map.set env ~key:x ~data:loc

  let local_environment (env : environment) (m : value M.t) : value M.t =
    M.(lift (run m env))

  let empty_effects () : unit M.t = M.lift @@ Effects.(put empty)
  let get_effects () : effect list M.t = M.lift @@ Effects.get ()
  let empty_states () : unit M.t = M.lift @@ Effects.lift @@ States.(put empty)
  let get_states () : state list M.t = M.lift @@ Effects.lift @@ States.get ()

  let mrun (m : value M.t) : result =
    let result, store =
      Store.(
        run
          ExnState.(run States.(run Effects.(run M.(run m empty) empty) empty))
          empty)
    in
    { result; store }

  let evaluate (eval : evaluation -> evaluation) (e : expression) : result =
    mrun ((fix eval) e)
end

module Default_interpreter = Make_interpreter (Default_components)
