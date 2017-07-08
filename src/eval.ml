module Ctx = struct
  type t = {
    typed_ctx: Typed.Ctx.t;
    lambda_ctx: Lambda.Ctx.t;
    interpret_ctx: Interpret.Ctx.t;
  }

  let empty = {
    typed_ctx = Typed.Ctx.empty;
    lambda_ctx = Lambda.Ctx.empty;
    interpret_ctx = Interpret.Ctx.empty;
  }
end

let step ctx s =
  let open Ctx in
  let parsed = Parsed_helpers.parse s in
  let typed = Typed.introduce_types parsed in
  let (typed', typed_ctx') = Typed.unify_and_substitute ~ctx:ctx.typed_ctx typed in
  let (lambda, lambda_ctx') = Lambda.transform_to_lambda ~ctx:ctx.lambda_ctx typed' in
  let (v, interpret_ctx') = Interpret.interpret ~ctx:ctx.interpret_ctx lambda in
  (v, { typed_ctx = typed_ctx'; lambda_ctx = lambda_ctx'; interpret_ctx = interpret_ctx' })

let eval s = step Ctx.empty s |> fst

let rec repl ?ctx:(ctx=Ctx.empty) () =
  print_string "> ";
  try
    let s = read_line () in
    let (v, ctx') = step ctx s in
    Interpret.format_value v |> print_endline;
    repl ~ctx:ctx' ()
  with
  | Parsed_helpers.Parser_helpers_exception ->
      Printf.printf "parsing error\n";
      repl ~ctx ()
  | Interpret.Interpret_exception error ->
      Printf.printf "interpreter error: %s\n" (Interpret.format_error error);
      repl ~ctx ()
  | Lambda.Lambda_exception error ->
      Printf.printf "lambda error: %s\n" (Lambda.format_error error);
      repl ~ctx ()
  | Typed.Typed_exception error ->
      Printf.printf "typed error: %s\n" (Typed.format_error error);
      repl ~ctx ()
  | End_of_file -> ()
