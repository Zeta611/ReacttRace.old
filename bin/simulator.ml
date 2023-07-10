open Stdio
open React_trace.Simulator

type box = Box : 'a -> box

let component () =
  let open React in
  let count, setCount = useState 0 in
  let text, setText = useState "" in
  useEffect0 (fun () -> printf "render\n");
  useEffect (fun () -> printf "no deps\n") [];
  useEffect (fun () -> printf "count\n") [ count ];
  useEffect (fun () -> printf "text\n") [ text ];
  useEffect (fun () -> printf "all deps\n") [ Box count; Box text ];
  object
    method render = printf "{ count : %d ; text : \"%s\" }\n" count text
    method click = setCount (count + 1)
    method write s = setText (text ^ s)
  end

let () =
  let open React in
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
