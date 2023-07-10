open Base
open Stdio
open React_trace.Interpreter

let expression =
     Application
       {
         func =
           Function
             {
               argument = "x";
               body = Function { argument = "y"; body = Variable "x" };
             };
         argument = Operator { operator = "+"; lhs = Constant 1; rhs = Constant 2 };
       }

   let { result; store } = Default_interpreter.evaluate expression

   let () =
     printf "expression: %s\n" (show_expression expression);
     (match result with
     | Ok value -> printf "value: %s\n" (show_value (fst @@ fst @@ value))
     | Error exn -> printf "exception: %s\n" (Exn.to_string exn));
     printf "store: %s\n" (show_store store)
