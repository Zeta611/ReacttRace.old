open Base
open Stdio
open React_trace
open Simulator.React
module Uint = Univ.Make_univ (Int)
module Ustring = Univ.Make_univ (String)

let component () =
  let count, setCount = useState (Uint.create 0) in
  let text, setText = useState (Ustring.create "") in

  useEffect0 (fun () -> printf "render\n");
  useEffect (fun () -> printf "no deps\n") [];
  useEffect (fun () -> printf "count\n") [ (count, Uint.( = )) ];
  useEffect (fun () -> printf "text\n") [ (text, Ustring.( = )) ];
  useEffect
    (fun () -> printf "all deps\n")
    [ (count, Uint.( = )); (text, Ustring.( = )) ];

  object
    method render =
      let count, text = (Uint.match_exn count, Ustring.match_exn text) in
      printf "{ count : %d ; text : \"%s\" }\n" count text

    method click =
      let count = Uint.match_exn count in
      setCount (Uint.create (count + 1))

    method write s =
      let text = Ustring.match_exn text in
      setText (Ustring.create (text ^ s))
  end

let () =
  printf "=== Deps test ===\n";
  reset ();
  let c = render component in
  c#click;
  let c = render component in
  c#click;
  c#write "Hello, ";
  let c = render component in
  c#click;
  let c = render component in
  c#write "React!";
  render component |> ignore

let great_grandchild () =
  useEffect0 (fun () -> printf "great-grandchild\n");
  object
    method render = ()
  end

let grandchild f =
  let show, setShow = useState (Uint.create 0) in
  useEffect0 (fun () ->
      printf "grandchild\n";
      f 0;
      setShow (Uint.create 1));
  if Uint.match_exn show = 0 then
    object
      method render = ()
    end
  else great_grandchild ()

let child f =
  useEffect0 (fun () ->
      printf "child\n";
      f 1);
  grandchild f

let parent () =
  let show, setShow = useState (Uint.create 1) in
  useEffect0 (fun () -> printf "parent\n");
  if Uint.match_exn show = 0 then
    object
      method render = ()
    end
  else child (fun x -> setShow (Uint.create x))

let () =
  reset ();
  printf "=== Chain test [WIP] ===\n";
  render parent |> ignore
